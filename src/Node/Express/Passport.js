const passport = require('passport')

exports._getPassport = function() {
  return require('passport')
}

exports._passportInitialize = function(passport, options) {
  return passport.initialize(options)
}

exports._passportSession = function(passport, options) {
  return passport.session(options)
}

exports._addSerializeUser = function(passport, serializer) {
  passport.serializeUser(serializer)
}

exports._addDeserializeUser = function(passport, deserializer) {
  passport.deserializeUser(deserializer)
}

exports._authenticate = function(passport, strategy, options, callback) {
  return
    passport.authenticate(
      strategy,
      options,
      (
        callback ?
        (
          function _authenticateCallback(err, user, info, status) {
            callback(
              err,
              user ? user : null, // can be false
              info ? info : null, // can be false
              status ? status : null // can be false
            )
          }
        ) : null
      )
    )
}

exports._isAuthenticated = function(req) {
  return req.isAuthenticated()
}

exports._logIn = function(req, user, options, done) {
  req.logIn(user, options, done)
}

exports._logOut = function(req) {
  req.logOut()
}

exports._getUser = function(req) {
  if (!(req._passport && req._passport.instance)) {
    throw new Error('passport is not initialized')
  }

  const property = req._passport.instance._userProperty

  if (!property) {
    throw new Error('req._passport.instance._userProperty is empty')
  }

  const user = req[property]

  return user ? user : null // can be false
}
