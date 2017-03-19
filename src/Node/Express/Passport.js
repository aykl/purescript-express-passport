
var passport = require('passport');

exports._getPassport = function() {
  return require('passport');
};

exports._passportInitialize = function(passport, options) {
  return function() {
    return passport.initialize(options);
  }
};

exports._passportSession = function(passport, options) {
  return function() {
    return passport.session(options);
  }
};

exports._addSerializeUser = function(passport, serializer) {
  return function() {
    passport.serializeUser(function(req, user, cb) {
      serializer(req, user, function(e, o) {
        return function() {
          return cb(e, o);
        }
      })();
    });
  }
};

exports._addDeserializeUser = function(passport, deserializer) {
  return function() {
    passport.deserializeUser(function(req, obj, cb) {
      deserializer(req, obj, function(e, u) {
        return function() {
          return cb(e, u);
        }
      })();
    });
  }
};

exports._authenticate = function(passport, strategy, options, callback) {
  return function(req, res, next) {
    return function() {
      var onAuthenticate = null;
      if (callback) {
        onAuthenticate = function(err, user, info, status) {
          if (user === false || user === undefined) user = null;
          if (info === false || info === undefined) info = null;
          if (status === false || status === undefined) status = null;
          callback(err, user, info, status)();
        }
      }
      passport.authenticate(strategy, options, onAuthenticate)(req, res, next);
    }
  }
};

exports._isAuthenticated = function(req) {
  return function() {
    return req.isAuthenticated();
  }
};

exports._logIn = function(req, user, options, done) {
  return function() {
    var onLogin = null;
    if (done) {
      onLogin = function(err) {
        done(err)();
      }
    }
    req.logIn(user, options, onLogin);
  }
};

exports._logOut = function(req) {
  return function() {
    req.logOut();
  }
};

exports._getUser = function(req) {
  return function() {
    var property = 'user';
    if (req._passport && req._passport.instance) {
      property = req._passport.instance._userProperty || 'user';
    }
    var user = req[property];

    if (user === false || user === null) {
      return null;
    }
    return user;
  }
};
