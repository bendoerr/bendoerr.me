@Grab('com.dropbox.sdk:dropbox-client:1.5.3')
import com.dropbox.client2.*
import com.dropbox.client2.session.*

def appKey = args[0]
def appSecret = args[1]
def outDir = args[2]
def maxAlbums = args.size() > 3 ? args[3] as Integer : null

def tokensFile = 'updatePhotos.groovy.tokens' as File
def photosHashFile = 'updatePhotos.groovy.hash' as File
def thumbsDir = new File(outDir, 'thumbs')

DropboxAPI api = new DropboxAPI(
    new WebAuthSession(
        new AppKeyPair(appKey, appSecret), Session.AccessType.DROPBOX
    )
)


if (tokensFile.exists()) {
    def tokens = tokensFile.text.tokenize(',')
    api.getSession().setAccessTokenPair(new AccessTokenPair(tokens[0], tokens[1]))
} else {
    String url = api.getSession().getAuthInfo().url
    println "Please go to this URL and hit 'Allow': ${url}. Press any key when done."
    Runtime.getRuntime().exec(['open', url] as String[])
    System.in.newReader().readLine()

    AccessTokenPair tokenPair = api.getSession().getAccessTokenPair()
    api.getSession().retrieveWebAccessToken(new RequestTokenPair(tokenPair.key, tokenPair.secret))

    tokensFile.text = api.getSession().getAccessTokenPair().key +
        "," + api.getSession().getAccessTokenPair().secret
}

def photosDir = api.metadata('/Photos', 0, null, true, null)
if (photosHashFile.exists() && photosHashFile.text == photosDir.hash) {
    println "Nothing has changed. Come back some other time."
    return 0
}

def links = [:]
//def revSortDirs = photosDir.contents.sort { RESTUtility.parseDate(it.modified) }.path
def revSortDirs = photosDir.contents*.path.sort().reverse()
def run = maxAlbums ? revSortDirs.take(maxAlbums) : revSortDirs
run.sort().each {albumPath->
    def albumLinks = []
    api.metadata(albumPath, 0, null, true, null).contents*.path.each {photoPath->
        def url = RESTUtility.request(
            RESTUtility.RequestMethod.GET,
            "api.dropbox.com",
            "shares/dropbox" + photoPath,
            DropboxAPI.VERSION,
            ["short_url","false"] as String[],
            api.getSession()
        ).url
        def f = new File(thumbsDir, photoPath)
        if (!f.exists()) {
            println "Downloading $photoPath"
            def dbin = api.getThumbnailStream(photoPath, DropboxAPI.ThumbSize.BESTFIT_320x240, DropboxAPI.ThumbFormat.JPEG)
            f.getParentFile().mkdirs()
            def fout = new FileOutputStream(f)
            dbin.copyStreamToOutput(fout, null)
            dbin.close()
            fout.close()
        }

        albumLinks << [photoPath, url]
    }
    links.put albumPath, albumLinks
}

def html = links.collect {albumPath, photoDets->
    """
<div class="row">
    <div class="span12">
      <h3>${albumPath.split('/').last()}</h3>
      <div class="well">
        <ul class="thumbnails">
          ${
            photoDets.collect {d->
                def path = d[0]
                def link = d[1].replace('www', 'dl')

                """
                <li class="span2">
                  <div class="thumbnail">
                  <a href="${link}" rel="lightbox[${albumPath.split('/').last()}]">
                    <div class="crop" style="background-image: url('thumbs${path}')">
                    </div>
                  </a>
                  </div>
                </li>
                """

            }.join('\n')
          }
        </ul>
      </div>
   </div>
</div>
    """
}.join('\n')

println html

assert false
// Last thing
photosHashFile.text = photosDir.hash
