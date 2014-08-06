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
    
    scope.sensResponse = [];
    scope.submitResponse = [];
    

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
  };

  module.exports = ['$scope', '$http', '$window', AppController];
})();




//(function() {
//	  'use strict';
//
//	  var AppController = function(scope, http, window) {
//
//	    var API_ROOT = window.appConfig.apiHost;
//
//	    // Model mirroring Submit model defined in ApiRoute.scala
//	    // Used for data binding and will be submitted as JSON when
//	    // user clicke ths 'Submit' button.
////	    scope.submit = {
////	      kp: null
////	    };
//	    
//	    scope.submit = {
//	      kp: null,
//	      disrel: null, 
//	      arg1: null, 
//	      arg2: null
//	    };
////	    scope.submit = {
////	      disrel: null
////	    }
//	    
//
//	    scope.submitToServer = function() {
//	      scope.insResponse = [];
//	      http.post(API_ROOT + '/submit', scope.submit).then(function(response) {
//	          scope.insResponse = response.data;
//	      });
//	    };
//	    
//	    scope.sumitToSearchSens = function(a1, a2) {
//	      scope.submit.arg1 = a1;
//	      scope.submit.arg2 = a2;
//	      scope.sensResponse = [];
//	      http.post(API_ROOT + '/submitins', scope.submit).then(function(response) {
//	    	scope.sensResponse = response.data;
//	      });
//	    };
//	  };
//
//	  module.exports = ['$scope', '$http', '$window', AppController];
//	})();

