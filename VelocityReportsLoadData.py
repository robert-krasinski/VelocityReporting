#!/usr/bin/env python
import csv
import datetime
import io
import os
import re
import string
from collections import namedtuple

from restkit import Resource, BasicAuth
# import urllib2
import json
import optparse
import sys
import time
from joblib import Parallel, delayed

reload(sys)
sys.setdefaultencoding('utf-8')

Component = namedtuple('Component', ['component', 'project'], verbose=False)

SprintIssue = namedtuple('SprintIssue', ['key', 'sprintId'], verbose=False)
ComponentIssue = namedtuple('ComponentIssue', ['key', 'component'], verbose=False)
#EpicIssue = namedtuple('EpicIssue', ['epic', 'issue'], verbose=False)
IssueLink = namedtuple('IssueLink', ['key', 'linkedKey'], verbose=False)

Issue = namedtuple('Issue',
                   ['id', 'project', 'key', 'summary', 'sprint', 'updated', 'priority', 'severity', 'component',
                    'fixVersion', 'minorVersion', 'type', 'created', 'reporter', 'status',
                    'devOwner', 'workTimeToComplete', 'timeToComplete', 'movedToComplete', 'timeoriginalestimate',
                    'aggregatetimeoriginalestimate', 'transitions', 'codeReviewToDev', 'testsToDev',
                    'remainingEstimate'], verbose=False)

Sprint = namedtuple('Sprint',
                    ['id', 'rapidViewId', 'state', 'name', 'dontKnowWhatExactly', 'startDate', 'endDate',
                     'completeDate', 'sequence'],
                    verbose=False)

IssueReviewer = namedtuple('IssueReviewer',
                           ['key', 'reviewer', 'approver'],
                           verbose=False)


class Fetcher(object):
    """ This factory will create the actual method used to fetch issues from JIRA. This is really just a closure that saves us having
        to pass a bunch of parameters all over the place all the time. """

    def __init__(self, url, auth):
        self.url = url
        self.auth = auth

    def getJSON(self, url):
        resource = Resource(url, filters=[auth])
        response = resource.get(headers={'Content-Type': 'application/json'})
        if response.status_int == 200:
            # Not all resources will return 200 on success. There are other success status codes. Like 204. We've read
            # the documentation for though and know what to expect here.
            versions = json.loads(response.body_string())
            return versions
        else:
            print response.status_int
            # print response.
            return None

    def getVersions(self, project):
        apiUrl = self.url + ('/rest/api/2/project/' + project + '/versions')
        print "Retrieving versions"
        print apiUrl
        return self.getJSON(apiUrl)

    def GetSprints(self, boardId, startAt):
        apiUrl = self.url + ('/rest/agile/1.0/board/' + boardId + '/sprint?startAt=' + str(startAt))
        print "Retrieving sprints"
        print apiUrl
        return self.getJSON(apiUrl)

    def GetIssuesInSprint(self, boardId, sprintId):

        if (boardId == '12'):
            project = 'VEL'
        elif boardId == '71':
            project = 'VBS'
        elif boardId == '91':
            project = 'VIN'
        else:
            raise ValueError('Unknown boardId: ' + str(boardId))

        apiUrl = self.url + ('/rest/api/2/search?jql=project+in+%28' + project + '%29+and+Sprint=' + str(sprintId))
        print "Retrieving issues in sprint"
        print apiUrl
        return self.getJSON(apiUrl)

    def GetComponentsForProject(self, project):
        apiUrl = self.url + '/rest/api/2/project/' + project + '/components'
        print "Retrieving components in project"
        print apiUrl
        return self.getJSON(apiUrl)

    def GetReviewersById(self, issueId, issueKey):
        apiUrl = self.url + '/rest/dev-status/1.0/issue/detail?issueId=' + issueId + '&applicationType=github&dataType=pullrequest'
        jsonResponse = self.getJSON(apiUrl)
        reviewersList = list()
        print 'reviewers ' + issueKey

        pullRequests = jsonResponse[u'detail'][0][u'pullRequests']
        for pull in pullRequests:
            for reviewer in pull['reviewers']:
                issueReviewer = IssueReviewer(
                    key=issueKey,
                    reviewer=reviewer['name'],
                    approver=reviewer['approved']

                )
                reviewersList.append(issueReviewer)
        return reviewersList

    def GetReviewers(self):
        # find latest file with issues
        files = [f for f in os.listdir('./data') if re.match(r'VelocityIssues.*\.csv', f)]
        files = sorted(files, reverse=True)
        latestFile = files[0]
        latestFile = './data/' + latestFile

        issueReviewers = list()
        with open(latestFile, 'rb') as csvfile:
            issuesReader = csv.reader(csvfile, delimiter=',', quotechar='.')
            issuesReader = iter(issuesReader)
            next(issuesReader)
            for row in issuesReader:
                issueId = row[0]
                issueKey = row[2]
                reviewers = self.GetReviewersById(issueId, issueKey)
                issueReviewers = issueReviewers + reviewers
                # iterate through issues to find pull requests
        # iterate through pull requests to find reviewers
        # add reviewer - issue

        return issueReviewers


def parseArgs():
    parser = optparse.OptionParser()
    parser.add_option('-u', '--user', dest='user', default='R.Krasinski', help='Username to access JIRA')
    parser.add_option('-p', '--password', dest='password', default='pass', help='Password to access JIRA')
    parser.add_option('-j', '--jira', dest='jira_url', default='https://kainos-evolve.atlassian.net',
                      help='JIRA Base URL')

    return parser.parse_args()


# decodes information from sprint
# example sprint info from jira api: u'com.atlassian.greenhopper.service.sprint.Sprint@438c36[id=140,rapidViewId=12,state=CLOSED,name=T1S24 - SAML,startDate=2016-03-31T13:47:01.180+01:00,endDate=2016-04-13T13:47:00.000+01:00,completeDate=2016-04-14T13:03:53.968+01:00,sequence=140]'
def DecodeSprintInfo(sprintEncoded):
    bracketPosition = string.find(sprintEncoded, "[")
    sprintEncoded = sprintEncoded[bracketPosition + 1:]
    sprintEncodedValues = string.split(sprintEncoded, ',')
    decodedSprintValues = []
    for sprintValue in sprintEncodedValues:
        keyValue = string.split(sprintValue, '=')
        if len(keyValue) < 2: continue
        # add value to list
        decodedSprintValues.append(keyValue[1])
        # create object from values list
    return Sprint._make(decodedSprintValues)


# in hours
def GetTimeInStatus(previousTransferDate, transferDate):
    if (previousTransferDate == None or transferDate == None): return None
    fromDate = datetime.datetime.strptime(previousTransferDate[:-9], "%Y-%m-%dT%H:%M:%S")
    toDate = datetime.datetime.strptime(transferDate[:-9], "%Y-%m-%dT%H:%M:%S")

    hours = round((toDate - fromDate).total_seconds() / 60 / 60, 3)
    print 'hours between ' + str(fromDate) + ' and ' + str(toDate) + " = " + str(hours)
    return hours


def GetWorkTimeInStatus(previousTransferDate, transferDate):
    if (previousTransferDate == None or transferDate == None): return None
    fromDate = datetime.datetime.strptime(previousTransferDate[:-9], "%Y-%m-%dT%H:%M:%S")
    toDate = datetime.datetime.strptime(transferDate[:-9], "%Y-%m-%dT%H:%M:%S")

    dayGenerator = (fromDate + datetime.timedelta(x + 1) for x in xrange((toDate - fromDate).days))

    if (fromDate.date() == toDate.date()):
        workHours = round((toDate - fromDate).total_seconds() / 60 / 60, 3)
    else:
        workHours = (sum(1 for day in dayGenerator if day.weekday() < 5) + 1) * 8

    print 'workhours between ' + str(fromDate) + ' and ' + str(toDate) + " = " + str(workHours)
    return workHours


def isMajorVersion(version):
    if re.search('UK', version['name']) == None:
        return False
    else:
        return True

def isMinorVersion(version):
    return not isMajorVersion(version)

def getMajorVersion(iterable, default=None):
    majorVersions = filter(isMajorVersion, iterable)
    if (len(majorVersions) > 1): raise ValueError("More than one major version found.")
    return majorVersions[0]['name'] if majorVersions else None


def getMinorVersion(iterable, default=None):
    minorVersions = filter(isMinorVersion, iterable)
    minorVersions = sorted(minorVersions)
    #if (len(minorVersions) > 1): raise ValueError("More than one minor version found.")
    return minorVersions[0]['name'] if minorVersions else None


# http://stackoverflow.com/questions/363944/python-idiom-to-return-first-item-or-none
def get_first(iterable, default=None):
    if iterable:
        for item in iterable:
            return item
    return default


def GetReviewersFromGithub():
    global f, w, row
    reviewers = jsonFetcher.GetReviewers()
    reviewersFilename = os.getcwd() + "/data/reviewers" + str(datetime.datetime.now()) + ".csv"
    reviewersFilename = reviewersFilename.replace(":", ".")
    with open(reviewersFilename, 'w') as f:
        w = csv.writer(f)
        w.writerow(('key', 'reviewer', 'approver'))  # field header
        for row in reviewers:
            # print row
            w.writerow(row)
    print reviewersFilename


def GetComponentsFromJira():
    global component, f, w, row
    # get all components
    components = list()
    projects = ['VEL', 'VBS', 'VIN']
    for project in projects:
        componentsJson = jsonFetcher.GetComponentsForProject(project)
        for componentJson in componentsJson:
            component = Component(
                component=componentJson['name'],
                project=project
            )
            components.append(component)
    componentsJsonFilename = os.getcwd() + "/data/components" + "_" + str(datetime.datetime.now()) + ".json"
    componentsJsonFilename = componentsJsonFilename.replace(":", ".")
    with open(componentsJsonFilename, 'w') as f:
        w = csv.writer(f)
        w.writerow(('component', 'project'))  # field header
        for row in components:
            # print row
            w.writerow(row)


def GetSprintIssuesFromJira():
    global startAt, sprint, issue, f, issuesFilename, w, row
    # get sprints list from Jira
    boardIds = ['12', '71', '91']
    sprintIssueList = list()
    for boardId in boardIds:
        sprintsFilename = os.getcwd() + "/data/sprints" + boardId + "_" + str(datetime.datetime.now()) + ".json"
        sprintsFilename = sprintsFilename.replace(":", ".")

        startAt = 0
        allSprintsJson = list()
        while True:

            sprintsJson = jsonFetcher.GetSprints(boardId, startAt)

            for sprint in sprintsJson['values']:
                sprintId = sprint['id']
                issuesInSprintsJson = jsonFetcher.GetIssuesInSprint(boardId, sprintId)
                for issue in issuesInSprintsJson['issues']:
                    sprintIssue = SprintIssue(
                        key=issue['key'],
                        sprintId=sprintId
                    )
                    sprintIssueList.append(sprintIssue)

            allSprintsJson = allSprintsJson + sprintsJson['values']
            startAt += 50
            if sprintsJson['isLast']: break;

        with io.open(sprintsFilename, 'w', encoding='utf-8') as f:
            f.write(unicode(json.dumps(allSprintsJson, ensure_ascii=False)))
        print sprintsFilename
    sprintIssuesFile = os.getcwd() + "/data/SprintIssue" + str(datetime.datetime.now()) + ".csv"
    issuesFilename = sprintIssuesFile.replace(":", ".")
    with open(sprintIssuesFile, 'w') as f:
        w = csv.writer(f)
        w.writerow(('key', 'sprintId'))  # field header
        for row in sprintIssueList:
            # print row
            w.writerow(row)
    print sprintIssuesFile





def GetJiraIssues():
    global startAt, issueComponents, component, sprint, issue, issuesFilename, f, w, row
    startAt = 0
    maxResults = 50
    issueList = list()
    issueComponents = list()
    preDevelopmentStates = ['Idea', 'Refinement', 'Tech Refinement & Est.', 'Backlog']
    finalStates = ['Completed', 'Rejected', 'Reviewed', 'Resolved', 'Closed', 'Awaiting Review', 'Frozen', 'Archived']
    linkedVxt = list()

    while True:
        url = options.jira_url + "/rest/api/2/search?jql=project+in+%28VEL,VBS,VIN%29+and+type+not+in+%28Epic%29+ORDER+BY+created+ASC&expand=changelog&startAt=" + str(
            startAt)
        # url = options.jira_url + "/rest/api/2/search?jql=id+in+%28VBS-667,VBS-807,VEL-1098,VEL-410%29+ORDER+BY+created+ASC&expand=changelog&startAt=" + str(startAt)

        print url
        response = jsonFetcher.getJSON(url)

        for jsonIssue in response['issues']:

            key = jsonIssue['key']
            issueId = jsonIssue['id']
            print key
            fields = jsonIssue['fields']

            fixVersionName = getMajorVersion(fields['fixVersions'])
            minorVersion = getMinorVersion(fields['fixVersions'])
            # fixVersionName = fix_version['name'] if (fix_version) else None

            component = get_first(fields['components'])
            componentName = component['name'] if (component) else None

            for comp in fields['components']:
                componentIssue = ComponentIssue(
                    key=key,
                    component=comp['name']
                )
                issueComponents.append(componentIssue)

            for linkedIssue in fields[u'issuelinks']:
                if 'inwardIssue' in linkedIssue:
                    issueKey = linkedIssue[u'inwardIssue'][u'key']
                else:
                    issueKey = linkedIssue[u'outwardIssue'][u'key']

                #if unicode.startswith(issueKey, 0, 3) != 'VXT': continue

                link = IssueLink(
                    key = key,
                    linkedKey = issueKey
                )
                linkedVxt.append(link)

            if (fields['customfield_10007']):
                sprintEncoded = fields['customfield_10007'][0]
            else:
                sprintEncoded = None

            if (sprintEncoded):
                sprint = DecodeSprintInfo(sprintEncoded)
            else:
                sprint = None

            if (fields['customfield_11500']):
                devOwner = fields['customfield_11500']['name']
            else:
                devOwner = None

            # severity = fields['customfield_11000']['value'] if ('customfield_11000' in fields.keys()) else None;
            severity = None

            previousTransferDate = None

            timeToComplete = 0
            workTimeToComplete = 0
            transitions = 0
            lastTransitionToCompleteState = None
            testsToDev = 0
            codeReviewToDev = 0

            for history in jsonIssue['changelog']['histories']:
                transferDate = history['created']

                for item in history['items']:
                    if item['field'] != 'status': continue;

                    timeInFromStatus = GetTimeInStatus(previousTransferDate, transferDate)
                    workTimeInFromStatus = GetWorkTimeInStatus(previousTransferDate, transferDate)
                    # if(previousTransferDate) : issue.timeInFromStatus = previousTransferDate - issue.transferDate
                    print item['fromString'] + ' -> ' + item['toString'] + " : " + str(timeInFromStatus)
                    # print timeInFromStatus

                    fromStatus = item['fromString']
                    toStatus = item['toString']

                    if fromStatus == 'In Testing' and toStatus == 'In Development': testsToDev += 1
                    if fromStatus == 'In Code review' and toStatus == 'In Development':  codeReviewToDev += 1





                    # if from is in final state not add time
                    # if from is in preDev state not add time


                    # don't calculate time spent on idle statuses http://stackoverflow.com/questions/4843158/check-if-a-python-list-item-contains-a-string-inside-another-string
                    if any(fromStatus in s for s in finalStates) or any(fromStatus in s for s in preDevelopmentStates):
                        print 'skipping time to complete - final/pre dev state'
                    else:
                        print 'added to time to complete'
                        timeToComplete += timeInFromStatus if (timeInFromStatus) else 0;
                        workTimeToComplete += workTimeInFromStatus if (workTimeInFromStatus) else 0;
                        transitions += 1

                    previousTransferDate = transferDate  # previousTransferDate = issue.transferDate

                    # don't track moves from final state to final state
                    if any(toStatus in s for s in finalStates) and not any(fromStatus in s for s in finalStates):
                        lastTransitionToCompleteState = transferDate
                        print 'updated lastTransitionToCompleteState'
                    else:
                        print 'skipping update lastTransitionToCompleteState'
                    break;

            # for history in issue['histories']:
            priority = fields['priority']
            issue = Issue(
                id=issueId,
                project=fields['project']['key'],
                key=key,
                summary=fields['summary'],
                sprint=sprint.name if (sprint) else None,
                updated=fields['updated'],
                component=componentName,
                priority=priority['name'] if (priority) else None,
                severity=severity,
                fixVersion=fixVersionName,
                minorVersion= minorVersion,
                type=fields['issuetype']['name'],
                created=fields['created'],
                reporter=fields['reporter']['displayName'],
                status=fields['status']['name'],
                devOwner=devOwner,
                timeToComplete=timeToComplete,
                workTimeToComplete=workTimeToComplete,
                movedToComplete=lastTransitionToCompleteState,
                aggregatetimeoriginalestimate=fields['aggregatetimeoriginalestimate'],  # in seconds,
                timeoriginalestimate=fields['aggregatetimeoriginalestimate'],
                transitions=transitions,
                codeReviewToDev = codeReviewToDev,
                testsToDev = testsToDev,
                remainingEstimate= fields['timeestimate']

            )
            issueList.append(issue)
        # break;
        # check if all is retrieved
        startAt = startAt + maxResults;
        if (startAt >= response['total']):
            break;
    issuesFilename = os.getcwd() + "/data/VelocityIssues" + str(datetime.datetime.now()) + ".csv"
    issuesFilename = issuesFilename.replace(":", ".")
    print issuesFilename
    with open(issuesFilename, 'w') as f:
        w = csv.writer(f)
        w.writerow(('id', 'project', 'key', 'summary', 'sprint', 'updated', 'priority', 'severity', 'component',
                    'fixVersion', 'minorVersion', 'type', 'created', 'reporter', 'status',
                    'devOwner', 'workTimeToComplete', 'timeToComplete', 'movedToComplete',
                    'aggregatetimeoriginalestimate',
                    'timeoriginalestimate', 'transitions', 'codeReviewToDev', 'testsToDev', 'remainingEstimate'))  # field header
        for row in issueList:
            # print row
            w.writerow(row)

    componentIssuesFilename = os.getcwd() + "/data/ComponentIssues" + str(datetime.datetime.now()) + ".csv"
    componentIssuesFilename = componentIssuesFilename.replace(":", ".")
    print componentIssuesFilename

    with open(componentIssuesFilename, 'w') as f:
        w = csv.writer(f)
        w.writerow(('key', 'component'))  # field header
        for row in issueComponents:
            # print row
            w.writerow(row)

    linkedVxtFilename = os.getcwd() + "/data/linkedVxt" + str(datetime.datetime.now()) + ".csv"
    linkedVxtFilename = linkedVxtFilename.replace(":", ".")
    print linkedVxtFilename

    with open(linkedVxtFilename, 'w') as f:
        w = csv.writer(f)
        w.writerow(('key', 'linkedKey'))  # field header
        for row in linkedVxt:
            # print row
            w.writerow(row)


def getConcatenatedVersions(fixVersions):
    versionString = ''
    for version in fixVersions:
        versionString += version['name'] + ' '
    return versionString



def GetVXTAndRelated():
    relatedIssuesList, issueList = GetVXTAndRelatedLists("/rest/api/2/search?jql=(project+%3D+%22VELOCITY+Product%22)&startAt=")

    #load related issues
    searchUrl = CreateUrlForRelatedIssues(relatedIssuesList)
    #searchUrl = "/rest/api/2/search?jql=id+in+%28VXT-219%29&startAt="

    relatedIssuesList2ndLevel, issueList2ndLevel = GetVXTAndRelatedLists(searchUrl)
    issueList += issueList2ndLevel
    relatedIssuesList += relatedIssuesList2ndLevel


    relatedIssuesList2ndLevel = [x for x in relatedIssuesList2ndLevel if x.linkedKey.find("VXT") == -1]
    relatedIssuesList2ndLevel = [x for x in relatedIssuesList2ndLevel if x.linkedKey.find("VMCM") == -1]
    relatedIssuesList2ndLevel = [x for x in relatedIssuesList2ndLevel if x.linkedKey.find("GLOUC") == -1]
    relatedIssuesList2ndLevel = [x for x in relatedIssuesList2ndLevel if x.linkedKey.find("II") == -1]
    searchUrl = CreateUrlForRelatedIssues(relatedIssuesList2ndLevel)
    relatedIssuesList3rdLevel, issueList3rdLevel = GetVXTAndRelatedLists(searchUrl)
    issueList += issueList3rdLevel




    issuesFilename = os.getcwd() + "/data/VXTAndRelated" + str(datetime.datetime.now()) + ".csv"
    issuesFilename = issuesFilename.replace(":", ".")
    print issuesFilename
    with open(issuesFilename, 'w') as f:
        w = csv.writer(f)
        w.writerow(('id', 'project', 'key', 'summary', 'sprint', 'updated', 'priority', 'severity', 'component',
                    'fixVersion', 'minorVersion', 'type', 'created', 'reporter', 'status',
                    'devOwner', 'workTimeToComplete', 'timeToComplete', 'movedToComplete',
                    'aggregatetimeoriginalestimate',
                    'timeoriginalestimate', 'transitions', 'codeReviewToDev', 'testsToDev', 'remainingEstimate'))  # field header
        for row in issueList:
            # print row
            w.writerow(row)

    epicIssueFilename = os.getcwd() + "/data/RelatedIssues" + str(datetime.datetime.now()) + ".csv"
    epicIssueFilename = epicIssueFilename.replace(":", ".")
    print epicIssueFilename

    with open(epicIssueFilename, 'w') as f:
        w = csv.writer(f)
        w.writerow(('key', 'linkedKey'))  # field header
        for row in relatedIssuesList:
            # print row
            w.writerow(row)
    pass


def CreateUrlForRelatedIssues(relatedIssuesList):
    searchUrl = "/rest/api/2/search?jql=id+in+%28"
    # get related issues
    for relation in relatedIssuesList:
        searchUrl += relation.linkedKey + ","

    # remove last ,
    searchUrl = searchUrl[:-1]
    searchUrl += "%29&startAt="
    return searchUrl


def GetVXTAndRelatedLists(searchUrl):
    # global startAt, issueComponents, component, sprint, issue, issuesFilename, f, w, row


        startAt = 0
        maxResults = 50
        issueList = list()
        relatedIssues = list()


        while True:
            url = options.jira_url + searchUrl + str(
                startAt)

            print url
            response = jsonFetcher.getJSON(url)

            for jsonIssue in response['issues']:

                key = jsonIssue['key']
                issueId = jsonIssue['id']
                print key
                fields = jsonIssue['fields']

                # fixVersionName = getMajorVersion(fields['fixVersions'])
                fixVersionName = getConcatenatedVersions(fields['fixVersions'])
                minorVersion = getMinorVersion(fields['fixVersions'])
                # fixVersionName = fix_version['name'] if (fix_version) else None


                for linkedIssue in fields[u'issuelinks']:
                    if 'inwardIssue' in linkedIssue:
                        issueKey = linkedIssue[u'inwardIssue'][u'key']
                    else:
                        issueKey = linkedIssue[u'outwardIssue'][u'key']

                    epicIssue = IssueLink(
                        key=key,
                        linkedKey=issueKey
                    )
                    relatedIssues.append(epicIssue)

                # for history in issue['histories']:
                priority = fields['priority']
                issueType = fields['issuetype']['name']

                if issueType == 'Epic' :
                    epicStoriesUrl = "/rest/api/2/search?jql=('Epic+Link'+in+%28" + key + "%29)&startAt="
                    epicRelatedIssues, epicChildrenList = GetVXTAndRelatedLists(epicStoriesUrl)
                    issueList += epicChildrenList
                    for epicChild in epicChildrenList:
                        epicIssue = IssueLink(
                            key=key,
                            linkedKey=epicChild.key
                        )
                        relatedIssues.append(epicIssue)

                issue = Issue(
                    id=issueId,
                    project=fields['project']['key'],
                    key=key,
                    summary=fields['summary'],
                    sprint=None,
                    updated=fields['updated'],
                    component=None,
                    priority=priority['name'] if (priority) else None,
                    severity=None,
                    fixVersion=fixVersionName,
                    minorVersion=minorVersion,
                    type=issueType,
                    created=fields['created'],
                    reporter=fields['reporter']['displayName'],
                    status=fields['status']['name'],
                    devOwner=None,
                    timeToComplete=None,
                    workTimeToComplete=None,
                    movedToComplete=None,
                    aggregatetimeoriginalestimate=fields['aggregatetimeoriginalestimate'],  # in seconds,
                    timeoriginalestimate=fields['aggregatetimeoriginalestimate'],
                    transitions=None,
                    codeReviewToDev=None,
                    testsToDev=None,
                    remainingEstimate=fields['timeestimate']

                )
                issueList.append(issue)
            # break;
            # check if all is retrieved
            startAt = startAt + maxResults;
            if (startAt >= response['total']):
                break;
        return relatedIssues, issueList


def createOutputFolder():
    dataOutputPath = os.getcwd() + '/data'
    if not os.path.isdir(dataOutputPath) :
        os.makedirs(dataOutputPath)


    pass

if __name__ == '__main__':
    (options, args) = parseArgs()

    start = time.time()

    # print options.user
    # print options.password

    # Basic Auth is usually easier for scripts like this to deal with than Cookies.
    auth = BasicAuth(options.user, options.password)

    jsonFetcher = Fetcher(options.jira_url, auth)


    #GetReviewersFromGithub()

    createOutputFolder()
    GetVXTAndRelated()
    GetJiraIssues()
    GetSprintIssuesFromJira()
    GetComponentsFromJira()

    end = time.time()
    print(end - start)
