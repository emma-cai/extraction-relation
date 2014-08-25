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
    
    // for Classification.html
    scope.classifiersubmit = {
    	sentence: null, 
    	arg1: null,
    	arg2: null
    };
    scope.ResponseForClassifier = [];
    
    
    
    // for Bootstrapping.html
    scope.submit = {
      kp: 'caused by',
      disrel: 'CAUSE'
    };
    
    scope.savedData = {
      kp: null,
      sens: null
    };
    
    scope.argsubmit = {
      disrel: null, 
      arg1: null,
      arg2: null
    };
    
    scope.confirmed = false;
    scope.rejected = false;
    scope.isshow = false;
    scope.confirmednum = 0;
    scope.rejectedItems = 0;
    scope.sensResponse = [];
//    var inputSubmitString = stringFromScala;
//    var submitObject = angular.fromJson(inputSubmitString);
//    // submitObject has keys "disrel" & "kp". submitObject.disrel / submitObject.kp.
    scope.judge = [];
    scope.submitResponse = [];
    scope.dependencyResponse = [];
    scope.processor = 'search';		//'search' or 'dependency'
    

    // functions for classification
    scope.classifySentence = function() {
    	scope.ResponseForClassifier = [];
    	http.post(API_ROOT + '/classifysentence', scope.classifier).then(function(response) {
    		scope.ResponseForClassifier = response.data;
    	});
    };
    
    
    
    scope.submitDisrel = function() {
      scope.submitResponse = [];
      scope.sensResponse = [];
      http.post(API_ROOT + '/submitdisrel', scope.submit).then(function(response) {
          scope.submitResponse = response.data;
      });
    };
    
    scope.setProcessors = function(x) {
        scope.processor = x;
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
    	 http.post(API_ROOT + '/submitdisrel', scope.submit).then(function(response) {
             scope.dependencyResponse = response.data;
         });
      };
   
    
    scope.isChecked = function(id) {
    	var match = false;
    	for(var i=0; i<scope.judge.length; i++) {
    		if(scope.judge[i] === id) {
    			match = true;
    		}
    	}
    	return match;
    };
    
    scope.sync = function(bool, x) {
    	if(bool) {
    		scope.judge.splice(0,0,x);
    	}else{
    		for(var i=0; i<scope.judge.length; i++) {
    			if(scope.judge[i] === x) {
    				scope.judge.splice(i, 1);
    			}
    		}
    	}
    };
    
    scope.savePositive = function() {
//    	scope.savedData.disrel = scope.submit.disrel;	//disrel is String
//    	scope.savedData.sens = document.write(scope.judge); //sens is Array[String]

    	scope.isshow = true;
    };
    
  };

  module.exports = ['$scope', '$http', '$window', AppController];
})();
