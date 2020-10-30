exports._passportStrategyLocal = function(options, verify) {
  return new require('passport-local').Strategy(
    Object.assign({ passReqToCallback: true }, options),
    verify
  );
};
