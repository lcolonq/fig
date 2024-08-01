let canvas = document.getElementById("lcolonq-canvas");
let socket = null;
let currentFrame = null;

async function decompress(blob) {
  let ds = new DecompressionStream("gzip");
  let stream = blob.stream();
  let out = await new Response(stream.pipeThrough(ds));
  return out.arrayBuffer();
}

function readCell(dv, base) {
  let cell = {};
  let o = base;
  if (dv.getUint8(o) == 0) {
    return [{type: "bg"}, o + 1];
  } else {
    cell.type = "fg";
    cell.custom = dv.getUint8(o + 1);
    cell.r = dv.getUint8(o + 2);
    cell.g = dv.getUint8(o + 3);
    cell.b = dv.getUint8(o + 4);
    cell.g0 = dv.getUint32(o + 5);
    if (dv.getUint8(o + 9) == 0) {
      return [cell, o + 10];
    } else {
      cell.g1 = dv.getUint32(o + 10);
      return [cell, o + 14];
    }
  }
}

function readKeyframe(dv, base) {
  let ret = [];
  let o = base;
  for (let idx = 0; idx < (64 * 64); ++idx) {
    let res = readCell(dv, o);
    ret.push(res[0]);
    o = res[1];
  }
  currentFrame = ret;
}

function readDiff(dv, base) {
  if (currentFrame) {
    let len = dv.getUint32(base);
    let o = base + 4;
    for (let idx = 0; idx < len; ++idx) {
      let x = dv.getUint8(o);
      let y = dv.getUint8(o + 1);
      let c = readCell(dv, o + 2);
      currentFrame[x + (y * 64)] = c[0];
      o = c[1];
    }
  }
}

function readPacket(dv) {
  if (dv.getUint8(0) == 0) {
    readKeyframe(dv, 1);
  } else {
    readDiff(dv, 1);
  }
}

function renderCellCanvas(ctx, x, y, c) {
  if (c && c.type === "fg") {
    let msg = c.g1 ? String.fromCodePoint(c.g0, c.g1) : String.fromCodePoint(c.g0);
    if (msg.trim().length) {
      ctx.fillStyle = "black";
      ctx.fillRect(13 * y, 13 * x, 13, 13);
      ctx.fillStyle = `rgba(${c.r}, ${c.g}, ${c.b}, 1.0)`;
      ctx.fillText(msg, 13 * y, 13 * x + 10);
    }
  }
}

function renderCanvas() {
  if (canvas.width != canvas.clientWidth) {
    canvas.width = canvas.clientWidth;
  }
  if (canvas.height != canvas.clientHeight) {
    canvas.height = canvas.clientHeight;
  }
  if (currentFrame) {
    let ctx = canvas.getContext("2d");
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.font = "12px Iosevka Comfy";
    for (let y = 0; y < 64; ++y) {
      for (let x = 0; x < 64; ++x) {
        renderCellCanvas(ctx, x, y, currentFrame[(x * 64) + y]);
      }
    }
  }
}

export const _startModel = () => {
  socket = new WebSocket("wss://colonq.computer/bullfrog/api/channel/listen/model");
  socket.addEventListener("open", (ev) => {
    console.log("connected");
  });
  socket.addEventListener("message", async (ev) => {
    let arr = await decompress(ev.data);
    let view = new DataView(arr);
    readPacket(view);
    renderCanvas();
  });
};
