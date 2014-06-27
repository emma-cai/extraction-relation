angular.module('extraction', ['ui.bootstrap']);

var ExtractionCtrl = function($scope, $http) {
  $scope.model = { };
  $scope.working = false

  $scope.examples = ["When some animals prepare for the long winters by storing food and going dormant it is called hibernation.",
    "A hand lens is used to view objects in more detail.",
    "Freezing involves changing water from its liquid state to its solid state ice by the removal of heat.",
    "Animals can not make their own food so they must eat to get nutrients."];

  $scope.submitText = function() {
    $scope.working = true
    $http.post("/general/text", $scope.model.text)
      .success(function(data, status, headers, config) {
        $scope.response = data;
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
};
