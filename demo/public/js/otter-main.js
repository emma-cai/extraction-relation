angular.module('otter-extraction', ['ui.bootstrap']);

var ExtractionCtrl = function($scope, $http) {
  $scope.model = { };
  $scope.working = false

  $scope.examples = [ "An acid is a liquid with a high number of hydrogen ions in it that can burn things.",
   "A bandage is a long piece of material used to cover a wound, or protect an injury.",
   "A contagious illness caused by bacteria or a virus.",
   "The black necked crane  is a large bird and medium-sized crane found in the Tibetan plateau.",
   "Mercury is a silvery-colored, toxic, metallic chemical element, liquid at room temperature, with atomic number 80 and symbol Hg."];

  $scope.submitText = function() {
    $scope.working = true
    $http.post("/definition", $scope.model.text)
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

var SearchCtrl = function($scope, $http) {
  $scope.model = { };
  $scope.working = false

  $scope.examples = [ "acid",
    "mercury",
    "pollution",
    "winter" ];

  $scope.submitText = function() {
    $scope.working = true
    $http.post("/searchterm", $scope.model.text)
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

