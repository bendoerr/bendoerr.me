package me.bendoerr.website.autophotos

import org.quartz.JobExecutionContext
import org.quartz.JobExecutionException
import com.dropbox.client2.DropboxAPI.*
import static com.dropbox.client2.DropboxAPI.*
import org.slf4j.*

class DropboxPoll extends DropboxJob {

    private static final Logger log = LoggerFactory.getLogger(DropboxPoll)

    @Override
    void execute(def dropboxApi, JobExecutionContext context) throws JobExecutionException {
        def callback = context.getMergedJobDataMap().get('callback')

        log.info('Checking for changes at {}.', context.getFireTime())

        Set<Entry> recentPhotos = findRecentlyUpdatedPhotos(dropboxApi)

        if (recentPhotos) {
            Set<Entry> updatedAlbums = extractRecentlyUpdatedAlbums(recentPhotos, dropboxApi)
            callback(recentPhotos, updatedAlbums)
        }
    }

    private Set<Entry> findRecentlyUpdatedPhotos(dropboxApi) {
        DeltaPage<Entry> delta = dropboxApi.delta(getCursor())

        log.info('Dropbox Delta Cursor: {}', delta.cursor)
        log.info('Dropbox Delta Entries: {}', delta.entries?.size())
        log.info('Dropbox Delta Has More: {}', delta.hasMore)

        Set<Entry> recentPhotos = []
        delta.entries*.metadata.each {entry->
            if (entry?.path?.startsWith(photosPath) && validExtensions.any { entry.path.endsWith(it) }) {
                recentPhotos += entry
            }
        }

        log.info('Found {} updated photos.', recentPhotos.size())
        if (recentPhotos.size)
            log.debug('Updated photos:\n\t{}', {
                def mSize = recentPhotos ? recentPhotos.path*.length().max() : 0
                recentPhotos.collect { "${it.path.padRight(mSize)} Added: ${it.clientMtime}  Modified: ${it.modified}" }.join('\n\t')}())

        storeCursor(delta.cursor)

        if (delta.hasMore) {
            recentPhotos.addAll(findRecentlyUpdatedPhotos(dropboxApi))
        }

        return recentPhotos
    }

    private Set<Entry> extractRecentlyUpdatedAlbums(Set<Entry> recentPhotos, dropboxApi) {
        Set<Entry> recentAlbums = (recentPhotos.collect {
            it.path.split('/').with {p-> p.take(p.size() - 1) }.join('/')
        } as Set).collect {
            dropboxApi.metadata(it, 0, null, true, null)
        }

        log.info('Found {} updated albums.', recentAlbums.size())
        log.debug('Updated albums:\n\t{}', {
            def mSize = recentAlbums ? recentAlbums.path*.length().max() : 0
            recentAlbums.collect { "${it.path.padRight(mSize)} Added: ${it.clientMtime}  Modified: ${it.modified}" }.join('\n\t')}())

        return recentAlbums
    }

    private String getCursor() {
        if (cursorFile.exists()) {
            cursorFile.text
        }
    }

    private void storeCursor(String cursor) {
        cursorFile.text = cursor
    }
}
