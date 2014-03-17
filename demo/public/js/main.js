angular.module('extraction', ['ui.bootstrap']);

var ExtractionCtrl = function($scope, $http) {
  $scope.model = { };
  $scope.working = false

  $scope.submitText = function() {
    $scope.working = true
    $http.post("/text", $scope.model.text)
      .success(function(data, status, headers, config) {
        $scope.response = data;
        $scope.responseString = angular.toJson(data, pretty=true);
        $scope.working = false
      });
  };

  $scope.submitUrl = function() {
    $scope.working = true
    $http.post("/url", $scope.model.url)
      .success(function(data, status, headers, config) {
        $scope.response = data;
        $scope.responseString = angular.toJson(data, pretty=true);
        $scope.working = false
      });
  };
};
