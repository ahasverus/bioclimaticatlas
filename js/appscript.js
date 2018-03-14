$(document).ready(function() {


  // Add Get Code GitHub link in NavBar

  $("#nav").append('<li class="github"><a href="https://github.com/ahasverus/bioclimaticatlas"><i class="fab fa-github"></i> Get Code</a></li>');


  // Update selected color palette (on click)

  $("#menu_species div").on("click", function() {
    var selection = $(this).attr("class");
    $("#color-sel_species").removeClass();
    $("#color-sel_species").addClass(selection);
    $("#color-sel_species").removeClass("color-list");
    var selclass = $("#color-sel_species").attr("class");
    $("#color_species").val(selclass);
    Shiny.onInputChange("color_species", selclass);
  });

  $("#menu_climate div").on("click", function() {
    var selection = $(this).attr("class");
    $("#color-sel_climate").removeClass();
    $("#color-sel_climate").addClass(selection);
    $("#color-sel_climate").removeClass("color-list");
    var selclass = $("#color-sel_climate").attr("class");
    $("#color_climate").val(selclass);
    Shiny.onInputChange("color_climate", selclass);
  });

  $("#menu_ecosystem div").on("click", function() {
    var selection = $(this).attr("class");
    $("#color-sel_ecosystem").removeClass();
    $("#color-sel_ecosystem").addClass(selection);
    $("#color-sel_ecosystem").removeClass("color-list");
    var selclass = $("#color-sel_ecosystem").attr("class");
    $("#color_ecosystem").val(selclass);
    Shiny.onInputChange("color_ecosystem", selclass);
  });

  $("#menu_network div").on("click", function() {
    var selection = $(this).attr("class");
    $("#color-sel_network").removeClass();
    $("#color-sel_network").addClass(selection);
    $("#color-sel_network").removeClass("color-list");
    var selclass = $("#color-sel_network").attr("class");
    $("#color_network").val(selclass);
    Shiny.onInputChange("color_network", selclass);
  });

  $("#menu_vulnerability div").on("click", function() {
    var selection = $(this).attr("class");
    $("#color-sel_vulnerability").removeClass();
    $("#color-sel_vulnerability").addClass(selection);
    $("#color-sel_vulnerability").removeClass("color-list");
    var selclass = $("#color-sel_vulnerability").attr("class");
    $("#color_vulnerability").val(selclass);
    Shiny.onInputChange("color_vulnerability", selclass);
  });

  // Update selected color palette (on App opening)

  var selclass = $("#color-sel_species").attr("class");
  $("#color_species").val(selclass);

  var selclass = $("#color-sel_climate").attr("class");
  $("#color_climate").val(selclass);

  var selclass = $("#color-sel_ecosystem").attr("class");
  $("#color_ecosystem").val(selclass);

  var selclass = $("#color-sel_network").attr("class");
  $("#color_network").val(selclass);

  var selclass = $("#color-sel_vulnerability").attr("class");
  $("#color_vulnerability").val(selclass);
});
