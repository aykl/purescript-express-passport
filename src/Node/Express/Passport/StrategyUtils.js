exports._useStrategy = function(passport, strategyId, strategy) {
  passport.use(strategyId, strategy);
};
