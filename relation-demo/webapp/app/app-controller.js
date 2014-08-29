(function() {
  'use strict';

  var AppController = function(scope, http, window) {

    var API_ROOT = window.appConfig.apiHost;
    
    // for Classification.html
    scope.classifiersubmit = {
    	sentence: '', 
    	arg1: '',
    	arg2: ''
    };
    scope.ResponseForClassifier = [];
    
    
    // for Bootstrapping.html
    scope.submit = {
      kp: 'caused by',
      disrel: 'CAUSE'
    };
    
    scope.argsubmit = {
      disrel: null, 
      arg1: null,
      arg2: null
    };
    
    // functions for classification
    scope.classifySentence = function() {
    	scope.ResponseForClassifier = [];
//    	console.log(scope.classifiersubmit);
//    	console.log(API_ROOT);
    	http.post(API_ROOT + '/classifysentence', scope.classifiersubmit).then(function(response) {
    		scope.ResponseForClassifier = response.data;
    	});
    };
    
    scope.showClassifierExample = function() {
    	scope.classifiersubmit.sentence = 'water and food are required for animals to get growth.';
    	scope.classifiersubmit.arg1 = 'water and food';
    	scope.classifiersubmit.arg2 = 'get growth';
    };
    
    scope.submitDisrel = function() {
      scope.submitResponse = [];
      scope.sensResponse = [];
      http.post(API_ROOT + '/submitdisrel', scope.submit).then(function(response) {
          scope.submitResponse = response.data;
      });
    };
    
  };

  module.exports = ['$scope', '$http', '$window', AppController];
})();
