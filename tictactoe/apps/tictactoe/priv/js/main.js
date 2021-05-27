var autoMode = false;
var cpuThinking = false;
var noside = true;
var cpuSerifu;
var userSerifu;
var troutes;
var troutStates;

window.addEventListener('load', (event) => {
  cpuSerifu = document.getElementById("cpu_serifu");
  userSerifu = document.getElementById("user_serifu");
  troutes = [
    document.getElementById("pos_1_1"),
    document.getElementById("pos_1_2"),
    document.getElementById("pos_1_3"),
    document.getElementById("pos_2_1"),
    document.getElementById("pos_2_2"),
    document.getElementById("pos_2_3"),
    document.getElementById("pos_3_1"),
    document.getElementById("pos_3_2"),
    document.getElementById("pos_3_3")
  ];
  troutStates = [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ];
  troutes.forEach(trout => {
    trout.onclick = function() {onClickTrout(trout)};
  });
  var reset_button = document.getElementById("reset_button")
  reset_button.onclick = function() {resetGameScene(); };
  resetGameScene();
});

function resetGameScene() {
  let helloText = "よろしくおねがいします";
  setCpuSerifu(helloText);
  setUserSerifu(helloText);
  troutStates = [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ];
  setTroutInitImage();
  cpuThinking = false;
  noside = false;
}

function setTroutInitImage() {
  troutes.forEach(trout => {
    trout.src = "./img/empty_position.png";
  });
}

function setTroutCpu(x, y) {
  var id = "pos_" + x + "_" + y;
  var node = document.getElementById(id);
  setStateFromId(id, 2);
  node.src = "./img/cpu_mark.png";
}

function onClickTrout(e) {
  console.log(e);

  // もうゲームが終わっている場合
  if (noside) {
    return;
  }

  var state = getStateFromId(e.id);
  // 初期状態以外は何もしない
  console.log("state is " + state);
  if (state !== 0) {
    return;
  }

  setCpuSerifu("うーん...");
  setUserSerifu("そこっ！");
  setStateFromId(e.id, 1);
  e.src = "./img/user_mark.png";

  if(autoMode) {
    dmyCpu();
  } else {
    callCpuThink();
  }
}

function setCpuSerifu(text) {
  cpuSerifu.innerText = text;

}

function setUserSerifu(text) {
  userSerifu.innerText = text;
}

function getStateFromId(id) {
  return troutStates[getStateIndexFromId(id)];
}

function setStateFromId(id, value) {
  troutStates[getStateIndexFromId(id)] = value;
}

function getStateIndexFromId(id) {
  var index = 0;
  switch(id) {
    case "pos_1_1":
      index = 0;
      break;
    case "pos_1_2":
      index = 1;
      break;
    case "pos_1_3":
      index = 2;
      break;
    case "pos_2_1":
      index = 3;
      break;
    case "pos_2_2":
      index = 4;
      break;
    case "pos_2_3":
      index = 5;
      break;
    case "pos_3_1":
      index = 6;
      break;
    case "pos_3_2":
      index = 7;
      break;
    case "pos_3_3":
      index = 8;
      break;
  }
  return index;
}


function dmyCpu() {
  positions = [
    [1,1],
    [1,2],
    [1,3],
    [2,1],
    [2,2],
    [2,3],
    [3,1],
    [3,2],
    [3,3]
  ];
  var moved = false;
  positions.forEach((position, index) => {
    if (!moved && troutStates[index] === 0) {
      setTroutCpu(position[0], position[1]);
      moved  = true;
    }
  });
}

function callCpuThink() {
  cpuThinking = true;
  var request = new XMLHttpRequest();
  var postData = createCpuThinkPostData();
  request.addEventListener("load", loadCpuThink);
  request.addEventListener("progress", updateProgressCpuThink);
  request.addEventListener("load", transferCompleteCpuThink);
  request.addEventListener("error", transferFailedCpuThink);
  request.open("POST", "http://localhost/cpu_think");
  request.setRequestHeader("Content-Type", "application/json");
  request.setRequestHeader("Accept", "application/json");
  console.log('post data: ', postData);
  request.responseType = 'json';
  request.send(postData);
}

function createCpuThinkPostData() {
  var jsonObject = new Object();
  jsonObject.troutes = troutStates;
  return JSON.stringify(jsonObject);
}

function loadCpuThink(evt) {
  setCpuSerifu("ダイブ!!!!!!!");
}

function updateProgressCpuThink (oEvent) {
  let min = 1;
  let max = 5;
  let serifuNo = Math.floor(Math.random() * (max - min) + min); 
  var text = "";
  switch(serifuNo) {
    case 1:
      text = "えーっと...";
      break;
    case 2:
      text = "うーん...";
      break;
    case 3:
      text = "これだと...";
      break;
    case 4:
      text = "まいったね...";
      break;
    case 5:
      text = "まじでまいったね...";
      break;
  }
  setCpuSerifu(text);
}

function transferCompleteCpuThink (evt) {
  setCpuSerifu("おらぁ！");
  let result = evt.currentTarget.response;
  if(result.succeed) {
    setTroutCpu(result.x,result.y);
  } 
  checkGameState();
  cpuThinking = false;
}

function transferFailedCpuThink(evt) {
  setCpuSerifu("思考停止しました。リセットしてください。。");
  cpuThinking = false;
}

function checkGameState() {
  let gamestate = calculateGameState(troutStates);
  var gameset = false;
  switch(gamestate){
    case 1:
      // user win
      setUserSerifu("や　　っ　　た　　ぜ　　！");
      setCpuSerifu("ち　　く　　し　　ょ　　う");
      gameset = true;
      break;
    case 2:
      // cpu win
      setCpuSerifu("や　　っ　　た　　ぜ　　！");
      setUserSerifu("ち　　く　　し　　ょ　　う");
      gameset = true;
      break;
  }

  if(gameset) {
    noside = true;
  }
}

function calculateGameState(troutStates) {
  var lines = [
    [0, 1, 2],
    [3, 4, 5],
    [6, 7, 8],
    [0, 3, 6],
    [1, 4, 7],
    [2, 5, 8],
    [0, 4, 8],
    [2, 4, 6]
  ];
  var winSide = 0;
  lines.forEach(line => {
    if (winSide === 0) {
      if(troutStates[line[0]] === troutStates[line[1]] &&
        troutStates[line[1]] === troutStates[line[2]]) {
          if( troutStates[line[0]] == 1) {
            winSide = 1;
          }
          if( troutStates[line[0]] == 2) {
            winSide = 2;
          }
      }
    }
  });
  return winSide;
}