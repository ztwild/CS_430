
onmessage = function(e){
  console.log("Recieving function " + e.data.name);
  console.log("Params: " + e.data.params);
  console.log("Body: " + e.data.body);
  console.log("Args: " + e.data.args);
  var millis = 5000;
  setTimeout(() => {
      let g = new Function(e.data.params, e.data.body);
      let z = g.apply(null, e.data.args);
      
      postMessage(z);
    }, millis);
  
};

