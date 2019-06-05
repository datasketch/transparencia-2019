$(document).on('click', '.needed', function () {
  Shiny.onInputChange('last_click',this.id);
});

$(document).on('click', '.click_option', function () {
  Shiny.onInputChange('last_option',this.id);
});

$(document).on('click', '.click_ficha', function () {
  Shiny.setInputValue("last_case", this.id, {priority: "event"});
  //Shiny.onInputChange('last_case',this.id);
});


$(document).on('click', '.needed', function(){
    $('.needed').removeClass('basic_active')
    $(this).addClass('basic_active');
});

$(document).on('click', '.click_option', function(){
  var target = event.target;
  var id = target.getAttribute('id');
  
  var avanzada = document.getElementById("cont_avanzada");
  var basicas = document.getElementById("cont_basicas");
  var red = document.getElementById("cont_red");
  var flecha_adv = document.getElementById("opts_a");
  var flecha_bsc = document.getElementById("opts");
  var flecha_red = document.getElementById("opts_r");
  var graficos = document.getElementById("type_viz");
  
  
  if (id == 'Avanzada') {
    avanzada.classList.remove("hideOptions");
    basicas.classList.add("hideOptions");
    graficos.classList.remove("hideOptions");
    red.classList.add("hideOptions");
    flecha_adv.classList.add("active_opt");
    flecha_red.classList.remove("active_opt");
    flecha_bsc.classList.remove("active_opt");
  } else if (id == 'Basico') {
    basicas.classList.remove("hideOptions");
    avanzada.classList.add("hideOptions");
    graficos.classList.remove("hideOptions");
    red.classList.add("hideOptions");
    flecha_adv.classList.remove("active_opt");
    flecha_red.classList.remove("active_opt");
    flecha_bsc.classList.add("active_opt");
  } else if (id == 'Red') {
    red.classList.remove("hideOptions");
    basicas.classList.add("hideOptions");
    avanzada.classList.add("hideOptions");
    graficos.classList.add("hideOptions");
    flecha_adv.classList.remove("active_opt");
    flecha_bsc.classList.remove("active_opt");
    flecha_red.classList.add("active_opt");
  } else {
    return
  }
})




