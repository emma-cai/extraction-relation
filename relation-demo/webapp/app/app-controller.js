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
      kp: null,
      disrel: null
    };
    
    scope.argsubmit = {
      disrel: null, 
      arg1: null,
      arg2: null
    };
    
//    scope.judge = [{"content":null, "istrue":'Y'}];
    scope.judge = null;
    
    scope.sensResponse = [];
    scope.submitResponse = [];
    scope.dependencyResponse = [];
    scope.processor = 'search';
    

    scope.submitToServer = function() {
      scope.submitResponse = [];
      scope.sensResponse = [];
      http.post(API_ROOT + '/submit', scope.submit).then(function(response) {
          scope.submitResponse = response.data;
      });
    };
    
  	scope.sumitToSearchSens = function(a1, a2) {
  	  scope.argsubmit.disrel = scope.submit.disrel;
      scope.argsubmit.arg1 = a1;
      scope.argsubmit.arg2 = a2;
      scope.sensResponse = [];
      http.post(API_ROOT + '/submitins', scope.argsubmit).then(function(response) {
  	    scope.sensResponse = response.data;
      });
    };
    
    
    scope.showDependency = function(x) {
    	scope.processor = 'dependency';
    	scope.submit.disrel = x;
        //TODO: search learned dependency patterns
    	scope.dependencyResponse = [];
//    	http.post(API_ROOT + '/submitdep', scope.submit.disrel).then(function(response) {
//     	   scope.dependencyResponse = response.data;
//        });
    	 http.post(API_ROOT + '/submitdep', scope.submit).then(function(response) {
             scope.dependencyResponse = response.data;
         });
      };
    
    /**
     * Judge the sentence, but it's not working now
     */
//    scope.confirm = function(sen, prejudge) {
//    	scope.judge = sen;
//    	if(prejudge == 'Y') {
//    		scope.judge[istrue] = 'N';
//    	}else {
//        	scope.judge[istrue] = 'Y';	
//    	}
//      };
  };

  module.exports = ['$scope', '$http', '$window', AppController];
})();

