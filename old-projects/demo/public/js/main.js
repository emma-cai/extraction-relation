angular.module('extraction', ['ui.bootstrap']);

var ExtractionCtrl = function($scope, $http) {
  $scope.model = { };
  $scope.working = false

  $scope.examples1 = ["cause"];
  $scope.examples2 = ["function of"]

  $scope.submitText = function() {
    $scope.working = true
    $http.post("/general/text", $scope.model.text)
      .success(function(data, status, headers, config) {
        $scope.response = data;
    	//  $scope.response = ["result1", "result2", "result3"];
        $scope.errorResponse = undefined;
        $scope.working = false
      })
      .error(function(data, status, headers, config) {
        $scope.response = undefined;
        $scope.errorResponse = data;
        $scope.errorResponse.status = status;
        $scope.working = false
      });
  };

  $scope.submitUrl = function() {
    $scope.working = true
    $http.post("/general/url", $scope.model.url)
      .success(function(data, status, headers, config) {
        $scope.response = data;
        $scope.working = false
      });
  };
  
  $scope.showExample = function(example) {
	  $scope.model.text = example;
	  $scope.submitText();
  };
  
  $scope.submitExample = function(example) {
	  $scope.model.text = example;
	  $scope.submitText();
  };
};
