exports._setStrategy = function(passport, name, strategy) {
  return function() {
    passport.use(name, strategy);
  }
};
