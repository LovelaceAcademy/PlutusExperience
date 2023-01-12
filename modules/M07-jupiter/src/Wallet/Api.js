export const enableWallet = window => name => () => window.cardano[name].enable();
export const getBalance = api => () => api.getBalance();
