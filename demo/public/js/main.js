angular.module('extraction', ['ui.bootstrap']);

var ExtractionCtrl = function($scope, $http) {
  $scope.model = { }

  $scope.submit = function(path) {
    $http.post("/" + path, $scope.model.text)
      .success(function(data, status, headers, config) {
        $scope.response = data;
        $scope.responseString = angular.toJson(data, pretty=true);
      })
  }
}
