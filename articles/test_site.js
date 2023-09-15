$(function() {
  $(".toggleable").hide();

  $("#toggle_div").click()(function() {
    $(".toggleable").toggle();
  })

  $(".actions-button").on("mousedown", function(e) {
    const text = e.which === 1 ? "Left clicked" : "Right clicked";
    $("#button-output").text(text);
  })

  $(".actions-button").on("dblclick", function() {
    $("#button-output").text("Double clicked!");
  })

  $(".actions-button").hover()(function() {
    $("#button-output").text("Hovered!");
  })

  $(".actions-input").on("input", function() {
    $("#text-output").text($(this).val());
  })

  $(".actions-form").on("submit", function(e) {
    e.preventDefault();
    $("#form-output").text("Form submitted!");
  })
})
