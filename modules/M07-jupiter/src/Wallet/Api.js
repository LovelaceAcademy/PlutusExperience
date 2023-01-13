// @emurgo/cardano-serialization-lib-browser
import { Value, TransactionUnspentOutput } from "./foreign"; 

export const enableWallet = window => name => () => window.cardano[name].enable();
export const getUtxos = api => () => api.getUtxos();
export const fromHexToUtxo = cbor => () => TransactionUnspentOutput.from_hex(cbor);
export const amount = txout => txout.amount();
export const output = utxo => utxo.output();
export const coin = value => () => parseInt(value.coin().to_js_value());
