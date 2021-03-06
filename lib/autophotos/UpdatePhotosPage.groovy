package me.bendoerr.website.autophotos

import com.dropbox.client2.*
import com.dropbox.client2.session.*
import static com.dropbox.client2.DropboxAPI.*
import org.slf4j.*

class UpdatePhotosPage extends DropboxJob {

    private static final Logger log = LoggerFactory.getLogger(UpdatePhotosPage)

    static final String CONTEXT_ATTR_PHOTOS = "recentPhotos"

    static final String CONTEXT_ATTR_ALBUMS = "recentAlbums"

    @Override
    void execute(def dropboxApi, org.quartz.JobExecutionContext context) throws org.quartz.JobExecutionException {
        def callback = context.getMergedJobDataMap().get('callback')
        def recentPhotos = context.getMergedJobDataMap().get(CONTEXT_ATTR_PHOTOS)
        def recentAlbums = context.getMergedJobDataMap().get(CONTEXT_ATTR_ALBUMS)

        log.info('Updating photos page ({}) at {}', photosPageFile.path, context.getFireTime())

        def albumDetails = buildRecentAlbumDetails(dropboxApi, recentAlbums)
        def html = photosPageFile.getText('UTF-8')

        int i = 0
        def toastHtml = albumDetails.collect {details->
            i++
            """\
            <div class="row">
                <div class="span12">
                    <h3>${details.name}</h3>
                    <div class="well">
                        <div id="photos$i">
                        </div>
                    </div>
                </div>
            </div>

            <script type="text/javascript">
                var photos$i = [
                    ${
                        details.photos.collect {
                            "{'width': $it.width, 'height': $it.height, 'thumbnail': '${it.thumbUrl}', 'large': '${it.originalUrl}'}"
                        }.join(',\n')
                    }
                ];
            </script>
            """

        }.join('\n')
        toastHtml += """\
            <script type="text/javascript">
                var photos = [${(1..i).collect { "'photos$it'" }.join(", ")}]
            </script>
        """

        def toast = '<!-- TOAST -->'
        def lines = html.tokenize('\n')
        def preToast = lines.takeWhile { !it.contains(toast) }
        def postToast = lines.reverse().takeWhile { !it.contains(toast) }.reverse()

        def t = preToast.join('\n') + "\n$toast\n" + toastHtml + "\n$toast\n" + postToast.join('\n')
        photosPageFile.setText(t, 'UTF-8')
        log.info('Running deployment command: {}', deployCmd)
        Runtime.getRuntime().exec(deployCmd.tokenize() as String[]).waitFor()
        log.info('Done updating photos page at {}.', new Date())
    }

    private buildRecentAlbumDetails(def dropboxApi, def recentAlbums) {
        recentAlbums.collect {albumEntry->
            def photosDetails = albumEntry.contents.collect {photoEntry->
                def originalUrl = sharePhoto(dropboxApi, photoEntry)
                def thumbSize = fetchThumbnail(dropboxApi, photoEntry)
                [
                    originalUrl: originalUrl,
                    thumbUrl: "thumbs${photoEntry.path}",
                    altText: photoEntry.path,
                    width: thumbSize.width,
                    height: thumbSize.height,

                ]
            }

            [
                name: albumEntry.path.split('/').last(),
                updated: albumEntry.modified,
                photos: photosDetails
            ]
        }
    }

    private String sharePhoto(def dropboxApi, Entry pEntry) {
        return RESTUtility.request(
                RESTUtility.RequestMethod.GET,
                "api.dropbox.com",
                "shares/dropbox" + pEntry.path,
                DropboxAPI.VERSION,
                ["short_url","false"] as String[],
                dropboxApi.getSession()
                ).url.replace('www', 'dl')
    }

    private Map fetchThumbnail(def dropboxApi, Entry pEntry) {
        def thumbFile = new File(thumbsDir, pEntry.path)
        if (!thumbFile.exists()) {
            log.debug('Downloading thumbnail of {}.', pEntry.path)
            thumbFile.parentFile.mkdirs()
            new FileOutputStream(thumbFile).with {out->
                def downloadStream = dropboxApi.getThumbnailStream(
                    pEntry.path,
                    DropboxAPI.ThumbSize.BESTFIT_320x240,
                    DropboxAPI.ThumbFormat.JPEG)
                downloadStream.copyStreamToOutput(out, null)
                downloadStream.close()
                out.close()
            }
        }
        def img = javax.imageio.ImageIO.read(thumbFile)
        [width: img.width, height: img.height]
    }


}
