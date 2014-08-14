(function() {
  'use strict';

  var AppController = function(scope, http, window) {

    var API_ROOT = window.appConfig.apiHost;

    // Model mirroring Submit model defined in ApiRoute.scala
    // Used for data binding and will be submitted as JSON when
    // user clicke ths 'Submit' button.
//    scope.submit = {
//      kp: null
//    };
    
    scope.submit = {
      qid: "",
      path: ""
    };
    
    scope.searchtype = "";
    
    scope.submitResponseInit = [];
    scope.submitResponseClassifier = [];
    scope.Submit = function(type) {
      if(type === "init") {
    	  scope.path = "/Users/qingqingcai/Documents/java/workspace/Hackthon/data/query-urls-sens-cleaned";
          scope.submitResponseInit = [];
          http.post(API_ROOT + '/submitinit', scope.submit).then(function(response) {
              scope.submitResponseInit = response.data;
          });  
      }else if(type === "classifier") {
    	  scope.path = "/Users/qingqingcai/Documents/java/workspace/Hackthon/data/query-urls-sens-classifier";
          scope.submitResponseClassifier = [];
          http.post(API_ROOT + '/submitclassifier', scope.submit).then(function(response) {
              scope.submitResponseInit = response.data;
          });
      }

    };
    
    
  };

  module.exports = ['$scope', '$http', '$window', AppController];
})();
