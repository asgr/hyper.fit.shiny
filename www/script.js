$(function() {

  Shiny.addCustomMessageHandler("showPlot", 
    function(message){
       $("#"+message).show();
    }
  );
  
  Shiny.addCustomMessageHandler("hidePlot", 
    function(message){
       $("#"+message).hide();
    }
  );

})