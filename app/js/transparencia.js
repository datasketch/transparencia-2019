$(document).on('click', '.needed', function () {
  Shiny.onInputChange('last_click',this.id);
});

$(document).on('click', '.click_option', function () {
  Shiny.onInputChange('last_option',this.id);
});