angular.module('extraction', ['ui.bootstrap']);

var ExtractionCtrl = function($scope, $http) {
  $scope.model = { };
  $scope.working = false

  $scope.examples = ["When some animals prepare for the long winters by storing food and going dormant it is called hibernation.",
    "A hand lens is used to view objects in more detail.",
    "Freezing involves changing water from its liquid state to its solid state ice by the removal of heat.",
    "Animals can not make their own food so they must eat to get nutrients.",
    "An abacus is an ancient calculating device.",
    "An acid is a liquid with a high number of hydrogen ions in it that can burn things.",
    "The black necked crane  is a large bird and medium-sized crane found in the Tibetan plateau.",
    "Bread is food made from mixing flour, water, and yeast.",
    "Mercury is a silvery-colored, toxic, metallic chemical element, liquid at room temperature, with atomic number 80 and symbol Hg."];

  $scope.submitText = function() {
    $scope.working = true
    $http.post("/text", $scope.model.text)
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
    $http.post("/url", $scope.model.url)
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
