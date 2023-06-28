export const flags = ({ env }) => {
  var profile = localStorage.profile
  var token = localStorage.token
  console.log("Loading...")

  var thing = {
    user: (profile && token) ? {
      profile: JSON.parse(profile),
      token,
    } : null
  }

  console.log("Something?")
  console.log(thing)

  return thing
}

export const onReady = ({ env, app }) => {
  var webAuth = new auth0.WebAuth({
    domain: 'dev-o5gx50q8iijtjzk5.eu.auth0.com', // e.g., you.auth0.com
    clientID: 'HzMXrEfveLdvV6k1497llBw4yI43hlPg',
    responseType: 'token',
    redirectUri: 'http://localhost:1234'
  });

  if (app.ports && app.ports.auth0authorize) {
    app.ports.auth0authorize.subscribe(() => {
      console.log('Authorizing...')
      webAuth.authorize({
        audience: 'https://auth0-jwt-authorizer'
      })
    })
  }

  if (app.ports && app.ports.auth0logout) {
    app.ports.auth0logout.subscribe(() => {
      console.log('Logging out...')
      localStorage.removeItem('profile')
      localStorage.removeItem('token')
    })
  }

  webAuth.parseHash({ hash: window.location.hash }, (err, authResult) => {
    if (err) {
      return console.error(err)
    }

    if (authResult) {
      webAuth.client.userInfo(authResult.accessToken, (err, profile) => {
        var result = { err: null, ok: null }
        var token = authResult.accessToken

        if (err) {
          result.err = err.details
          result.err.name = result.err.name ? result.err.name : null;
          result.err.code = result.err.code ? result.err.code : null;
          result.err.statusCode = result.err.statusCode ? result.err.statusCode : null;
        }

        if (authResult) {
          result.ok = { profile: profile, token: token }
          localStorage.setItem('profile', JSON.stringify(profile));
          localStorage.setItem('token', token);
        }

        console.log(result);
        app.ports.auth0authResult.send(result);
      })

      window.location.hash = ''
    }
  })
}
