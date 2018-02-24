$(document).ready(function() {


  // Add Get Code GitHub link in NavBar

  $("#nav").append('<li class="github"><a href="https://github.com/ahasverus/shiny-tundra-cc"><i class="fab fa-github"></i> Get Code</a></li>');


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


  // Update selected color palette (on App opening)

  var selclass = $("#color-sel_species").attr("class");
  $("#color_species").val(selclass);

  var selclass = $("#color-sel_climate").attr("class");
  $("#color_climate").val(selclass);
});
