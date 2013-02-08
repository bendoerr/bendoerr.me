package me.bendoerr.website.autophotos

import com.dropbox.client2.*
import com.dropbox.client2.session.*
import org.quartz.*
import org.quartz.impl.StdSchedulerFactory
import static org.quartz.DateBuilder.*
import static org.quartz.DateBuilder.IntervalUnit.*
import static org.quartz.JobBuilder.*
import static org.quartz.SimpleScheduleBuilder.*
import static org.quartz.TriggerBuilder.*
import static org.quartz.TriggerKey.*

// Quartz STFU!
System.setProperty('org.slf4j.simpleLogger.log.org.quartz', 'warn')

// YOLO
System.setProperty('org.slf4j.simpleLogger.log.me.bendoerr', 'debug')
System.setProperty('org.slf4j.simpleLogger.showThreadName', 'false')
System.setProperty('org.slf4j.simpleLogger.showShortLogName', 'true')

// Parse Command Line Options Like a Boss
def options = new CliBuilder(usage: 'autophotos [options]').with {
    _ longOpt: 'appkey',    args: 1, argName: 'key',     required: true,  'Dropbox App Key.'
    _ longOpt: 'appsecret', args: 1, argName: 'secret',  required: true,  'Dropbox App Secret.'
    h longOpt: 'help',                                                    'This Help Message.'
    _ longOpt: 'poll',      args: 1, argName: 'minutes',                  'Time between checking Dropbox for changes.'
    _ longOpt: 'delay',     args: 1, argName: 'minutes',                  'Time after detecting a change before updating webstie.'
    _ longOpt: 'page',      args: 1, argName: 'path',    required: true,  'Path to the page.'
    _ longOpt: 'runnow',                                                  'Ignore initial delays and just run it now.'
    _ longOpt: 'deploycmd', args: 1, argName: 'cmd',                      'Default: make build'
    _ longOpt: 'siteroot',  args: 1, argName: 'path',                     'If no --deploycmd is given augments make with --C'

    def opts = parse(args)

    if (!opts)
        return

    if (opts.help) {
        usage()
        return
    }

    return opts
}

if (!options)
    return 1

// Central Settings
Object.metaClass.getPhotosPath = {-> '/Photos' }
Object.metaClass.getValidExtensions = {-> ['jpg', 'png', 'gif'] }
Object.metaClass.getCursorFile = {-> '.autophotos.dropbox.cursor' as File }
Object.metaClass.getTokenFile = {-> '.autophotos.dropbox.token' as File }
Object.metaClass.getPhotosPageFile = {-> options.page as File }
Object.metaClass.getThumbsDir = {-> new File(getPhotosPageFile().parentFile, 'thumbs') }
Object.metaClass.getDeployCmd = {-> options.deploycmd ?: "make ${options.siteroot ? '-C ' + options.siteroot : ''} build"}

// Some Sane Defaults (debuggers might want to use --poll 1 --delay 1)
def poll = options.poll ? options.poll as Integer : 10
def delay = options.delay ? options.delay as Integer : 1440

// Get Dropbox Revved Up
def dropboxApi = new DropboxApiWrapper(options.appkey, options.appsecret)
dropboxApi.auth()

// Schedule The Two Jobs
def scheduler = new StdSchedulerFactory().getScheduler()
def qGroupId = 'autophotos'
def qPollId = 'dropboxPoll'
def qUpdateId = 'updateHtml'
def qDeployId = 'deploySite'

def pollCallback = {recentPhotos, recentAlbums->
    def tk = triggerKey("${qUpdateId}Trigger", qGroupId)
    def prev = scheduler.getTrigger(tk)
    def photos = recentPhotos
    def albums = recentAlbums

    if (prev && !prev.getPreviousFireTime()) {
        if (prev.jobDataMap.get(UpdatePhotosPage.CONTEXT_ATTR_PHOTOS))
            photos.addAll prev.jobDataMap.get(UpdatePhotosPage.CONTEXT_ATTR_PHOTOS)
        if (prev.jobDataMap.get(UpdatePhotosPage.CONTEXT_ATTR_ALBUMS))
            albums.addAll prev.jobDataMap.get(UpdatePhotosPage.CONTEXT_ATTR_ALBUMS)
    }

    scheduler.rescheduleJob(
        tk,
        newTrigger()
        .withIdentity(tk)
        .withSchedule(
            simpleSchedule()
                .withRepeatCount(0))
        .forJob("${qUpdateId}Job", qGroupId)
        .usingJobData(new JobDataMap(
                (UpdatePhotosPage.CONTEXT_ATTR_PHOTOS): photos,
                (UpdatePhotosPage.CONTEXT_ATTR_ALBUMS): albums))
        .with {
            if (options.runnow)
                startNow()
            else
                startAt(futureDate(delay, MINUTE))
        }
        .build())
}

scheduler.start()
scheduler.scheduleJob(
    newJob(DropboxPoll)
        .withIdentity("${qPollId}Job", qGroupId)
        .usingJobData(new JobDataMap(
            (DropboxJob.CONTEXT_ATTR_API): dropboxApi,
            callback: pollCallback ))
        .build(),
    newTrigger()
        .withIdentity("${qPollId}Trigger", qGroupId)
        .withSchedule(
            simpleSchedule()
                .withIntervalInMinutes(poll)
                .repeatForever())
        .with {
            if (options.runnow)
                startNow()
            else
                startAt(futureDate(poll, SECOND))
        }
        .build()
)
scheduler.scheduleJob(
    newJob(UpdatePhotosPage)
        .withIdentity("${qUpdateId}Job", qGroupId)
        .usingJobData(new JobDataMap(
            (DropboxJob.CONTEXT_ATTR_API): dropboxApi))
        .build(),
    newTrigger()
        .withIdentity("${qUpdateId}Trigger", qGroupId)
        .withSchedule(
            simpleSchedule()
                .withRepeatCount(0))
        .startAt(futureDate(1, YEAR))
        .build()
)

