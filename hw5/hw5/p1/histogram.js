onmessage = function(e){
  var results = new Int32Array(e.data.max);
  for(let i = 0; i < e.data.samples; i++){
    let next = Math.floor(Math.random() * e.data.max);
    results[next]++;
  }
  console.log("done");
  postMessage(results);
};