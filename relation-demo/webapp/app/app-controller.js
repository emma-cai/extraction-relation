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
    scope.confirmed = false;
    scope.judge = [];
    scope.isshow = false;
    
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
    
    scope.judgesubmit = {
    	qid: "",
    	positive: []
    };
    
    scope.savePositive = function() {
    	console.log(scope.judge);
    	scope.judgesubmit.qid = scope.submit.qid;
    	scope.judgesubmit.positive = scope.judge;
    	http.post(API_ROOT + '/submitjudge', scope.judgesubmit).then(function(response){
    		scope.judgeResp = response.data;
    	});
 //   	(String, Array[String])
    	scope.isshow = true;
    };
    
  };

  module.exports = ['$scope', '$http', '$window', AppController];
})();
