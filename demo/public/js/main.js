angular.module('extraction', ['ui.bootstrap']);

var ExtractionCtrl = function($scope, $http) {
  $scope.model = { };

  $scope.submitText = function() {
    $http.post("/text", $scope.model.text)
      .success(function(data, status, headers, config) {
        $scope.response = data;
        $scope.responseString = angular.toJson(data, pretty=true);
      });
  };

  $scope.submitUrl = function() {
    $http.post("/url", $scope.model.url)
      .success(function(data, status, headers, config) {
        $scope.response = data;
        $scope.responseString = angular.toJson(data, pretty=true);
      });
  };
};
