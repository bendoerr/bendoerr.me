package me.bendoerr.website.autophotos

import com.dropbox.client2.*
import com.dropbox.client2.session.*

class DropboxApiWrapper {

    private static String TOKEN_TOKEN = ',,'

    @Delegate
    DropboxAPI dropboxApi

    DropboxApiWrapper(String appKey, String appSecret) {
        dropboxApi = new DropboxAPI(
            new WebAuthSession(
                new AppKeyPair(appKey, appSecret), Session.AccessType.DROPBOX
            )
        )
    }

    void auth() {
        if (hasTokens()) {
            getSession().setAccessTokenPair(new AccessTokenPair(tokenKey, tokenSecret))
        } else {
            String url = getSession().getAuthInfo().url

            println "Please go to this URL and hit 'Allow': ${url}. Press any key when done."
            Runtime.getRuntime().exec(['open', url] as String[])
            System.in.newReader().readLine()

            AccessTokenPair tokenPair = getSession().getAccessTokenPair()
            getSession().retrieveWebAccessToken(new RequestTokenPair(tokenPair.key, tokenPair.secret))

            storeTokens()
        }
    }

    private boolean hasTokens() {
        tokenFile.exists() && tokenFile.text.tokenize(TOKEN_TOKEN).size() == 2
    }

    private String getTokenKey() {
        tokenFile.text.tokenize(TOKEN_TOKEN)[0]
    }

    private String getTokenSecret() {
        tokenFile.text.tokenize(TOKEN_TOKEN)[1]
    }

    private void storeTokens() {
        tokenFile.text = getSession().getAccessTokenPair().key +
                          TOKEN_TOKEN +
                          getSession().getAccessTokenPair().secret
    }
}

abstract class DropboxJob implements org.quartz.Job {

    static final String CONTEXT_ATTR_API = "dropboxApi"

    @Override
    void execute(org.quartz.JobExecutionContext context) throws org.quartz.JobExecutionException {
        def dropboxApi = context.getMergedJobDataMap().get(CONTEXT_ATTR_API)

        try {
            execute(dropboxApi, context)
        } catch (com.dropbox.client2.exception.DropboxException e) {
            if (e.message?.startsWith('<html>')) {
                Runtime.getRuntime().exec(['open', "data:text/html, $e.message"])
                throw e
            }
        }
    }

    abstract void execute(def dropboxApi, org.quartz.JobExecutionContext context) throws org.quartz.JobExecutionException

}
