
var Strategy = require('passport-local').Strategy;


exports._passportStrategyLocal = function(options) {
  return function(verify) {
    return new Strategy(
      Object.assign({ passReqToCallback: true }, options),
      function(req, username, password, verified) {
        verify(req, username, password, function(error, user, info) {
          return function() {
            verified(error, user, info);
          }
        })();
      }
    );
  }
};
