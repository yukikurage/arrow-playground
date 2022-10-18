export const newEventEmitter = () => ({
  listeners: new Set(),
})

export const emit = (emitter) => (ev) => () => {
  emitter.listeners.forEach((listener) => listener(ev)())
}

export const listen = (emitter) => (listener) => () => {
  emitter.listeners.add(listener)
  return undefined;
}