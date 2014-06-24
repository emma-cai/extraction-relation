angular.module('otter-extraction', ['ui.bootstrap']);

var SearchCtrl = function($scope, $http) {
  $scope.model = { };
  $scope.working = false

  $scope.examples = [ "acid",
    "mercury",
    "pollution",
    "winter" ];

  $scope.submitText = function() {
    $scope.working = true
    $http.post("/otter/text", $scope.model.text)
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

  $scope.showExample = function(example) {
    $scope.model.text = example;
    $scope.submitText();
  };
};

