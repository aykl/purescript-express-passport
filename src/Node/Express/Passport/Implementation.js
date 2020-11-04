exports.getPassport = function() {
  return require('passport')
}

exports._passportInitialize = function(passport, options) {
  return passport.initialize(options)
}

exports._passportSession = function(passport, options) {
  return passport.session(options)
}

exports._isAuthenticated = function(req) {
  return req.isAuthenticated()
}

exports._logOut = function(req) {
  req.logOut()
}
