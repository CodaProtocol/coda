const fs = require("fs");
const oldBlocks = JSON.parse(fs.readFileSync("parsed-blocks.json", "utf8"));
const newBlocks = JSON.parse(
  fs.readFileSync("missing-best-tip-logs-2020-11-19T02-16.json", "utf8")
);

newBlocksMap = new Map();
oldBlocksMap = new Map();

oldBlocks.forEach((block) => {
  const stateHash = block.stateHash;
  oldBlocksMap.set(stateHash, block);
});

newBlocks.forEach((payload) => {
  const { metadata } = payload.jsonPayload;
  const stateHash = metadata.added_transitions[0].state_hash;
  let epochData =
    metadata.added_transitions[0].protocol_state.body.consensus_state
      .next_epoch_data;

  // Rename keys
  epochData["epochLength"] = epochData["epoch_length"];
  delete epochData.epoch_length;

  epochData["lockCheckpoint"] = epochData["lock_checkpoint"];
  delete epochData.lock_checkpoint;

  epochData["startCheckpoint"] = epochData["start_checkpoint"];
  delete epochData.start_checkpoint;

  epochData.ledger["totalCurrency"] = epochData.ledger["total_currency"];
  delete epochData.ledger.total_currency;

  newBlocksMap.set(stateHash, epochData);
});

let blocksToWrite = [];

oldBlocksMap.forEach((block, stateHash) => {
  if (newBlocksMap.has(stateHash)) {
    const epochData = newBlocksMap.get(stateHash);
    block.protocolState.consensusState["nextEpochData"] = epochData;
  }
  blocksToWrite.push(block);
});

//console.log(blocksToWrite);

fs.writeFile(
  "updated-blocks.json",
  JSON.stringify(blocksToWrite, null, 4),
  function (err) {
    if (err) throw err;
    console.log("SUCCESS: Written blocks");
  }
);
