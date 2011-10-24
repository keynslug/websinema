/**
    Copyright (c) 2011 Drimmi, Inc.
    All rights reserved.
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
    
    http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

Ext.define('Websinema.Websocket', {
    
    extend: 'Ext.util.Observable',
    singleton: true,
    target: '/endpoint',
    passOrigin: true,
    
    constructor: function (config) {
        console.debug('Initiating websockets...');
        
        this.addEvents('online', 'offline', 'message');
        this.factory = this.prepareWebsocket();
        if (!this.factory) {
            console.error('Websockets are not supported.');
        }
        
        return this.superclass.constructor.call(this, config);
    },
    
    init: function (listeners) {
        var here = 'ws://' + window.location.host + this.target;
        console.debug('Estabilishing connection to: ', here);
        
        for (var event in listeners) {
            if (listeners.hasOwnProperty(event)) {
                if (event == 'scope') {
                    continue;
                }
                this.addListener(event, listeners[event], listeners.scope);
            }
        }
        
        if (this.factory) {
            try {
                this.socket = new this.factory(here);
                this.socket.binaryType = 'arraybuffer';
                this.socket.onopen = Ext.bind(this.onOpen, this);
                this.socket.onclose = Ext.bind(this.onClose, this);
                this.socket.onmessage = Ext.bind(this.onNotify, this);
            } 
            catch (e) {
                this.fireEvent('offline', 'invalid');
            }
        } else {
            this.fireEvent('offline', 'unsupported');
        }
    },
    
    errorCodes: {
        noBinary: 'Binaries are not supported.',
        invalidObject: 'Object received is invalid.',
        internal: 'Internal error: '
    },
    
    handlerStack: [],
    
    pushHandler: function (handler) {
        if (0 == this.handlerStack.length) {
            this.socket.onmessage = Ext.pass(this.onMessage, [handler], this);
        }
        else {
            this.handlerStack.push(handler);
        }
    },
    
    popHandler: function () {
        if (0 == this.handlerStack.length) {
            this.socket.onmessage = Ext.bind(this.onNotify, this);
        }
        else {
            var handler = this.handlerStack.unshift();
            this.socket.onmessage = Ext.pass(this.onMessage, [handler], this);
        }
    },
    
    send: function (payload, handler) {
        this.pushHandler(handler);
        if (Ext.isObject(payload)) {
            this.socket.send(Ext.JSON.encode(payload));
        }
        else {
            this.socket.send(payload.toString());
        }
    },
    
    receive: function (payload) {
        var message, view, object, handler, error = {
            status: false
        };
        
        try {
            if (Ext.isString(payload)) {
                message = payload;
            }
            else {
                if (this.prepareTypedArrays()) {
                    view = new Uint8Array(payload);
                    message = String.fromCharCode.call(String, Ext.Array.slice(view));
                }
                else {
                    error.reason = this.errorCodes.noBinary;
                }
            }
            if (message) {
                if (object = Ext.JSON.decode(message, true)) {
                    // blank
                } else {
                    error.reason = this.errorCodes.invalidObject;
                }
            }
        } catch (e) {
            error.reason = this.errorCodes.internal + e.toString();
        }
        
        object = object || error;
        if (this.passOrigin) {
            object.origin = payload;
        }
        
        return object;
    },
    
    prepareWebsocket: function () {
        if ('MozWebSocket' in window) {
            WebSocket = MozWebSocket;
        }
        if ('WebSocket' in window) {
            return WebSocket;
        }
    },
    
    prepareTypedArrays: function () {
        if ('ArrayBuffer' in window) {
            if ('Uint8Array' in window) {
                return true;
            }
        }
        return false;
    },
    
    onOpen: function () {
        console.debug("Connection was estabilished.");
        this.fireEvent('online');
    },
    
    onClose: function () {
        console.debug("Connection was closed.");
        this.fireEvent('offline', 'disconnected');
    },
    
    onMessage: function (handler, e) {
        console.debug("Connection received a reply.");
        this.popHandler();
        handler(this.receive(e.data));
    },
    
    onNotify: function (e) {
        console.debug("Connection received a portion of data.");
        this.fireEvent('message', this.receive(e.data), e.origin);
    }
    
});
