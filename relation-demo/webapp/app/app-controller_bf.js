(function() {
  'use strict';

  var AppController = function(scope, http, window) {

    var API_ROOT = window.appConfig.apiHost;

    // Model mirroring Submit model defined in ApiRoute.scala
    // Used for data binding and will be submitted as JSON when
    // user clicke ths 'Submit' button.
    scope.submit = {
      text: null
    };

    scope.submitToServer = function() {
      http.post(API_ROOT + '/submit', scope.submit).then(function(response) {
        scope.submitResponse = response.data;
      });
    };
  };

  module.exports = ['$scope', '$http', '$window', AppController];
})();
