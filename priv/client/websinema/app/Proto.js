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

Ext.define('Websinema.Proto', {
    
    extend: 'Ext.util.Observable',
    socket: Websinema.Websocket,
    
    constructor: function (config) {
        Ext.apply(this, config, {name: 'Proto'});
        console.debug(Ext.String.format('Initializing new protocol instance {0}...', this.name));

        this.handlers.scope = this;
        this.initProtocol();        
        this.addEvents('refresh', 'metrics');        
        this.socket.on('message', this.dispatchData, this);
        this.superclass.constructor.call(this, config);
    },
    
    proto: {
        
        getAgents: function () { return { a: 'agents_list' } },
        
        discoverAgent: function (name) { return { a: 'discover', name: name } },
        
        subscribe: function (interval, name, metric, on) {
            var sids = [];
            if (name && metric) {
                sids.push({a: name, sid: metric, on: on});
            }
            return {
                a: 'subscribe',
                i: interval,
                sids: sids
            }
        }

    },
    
    initProtocol: function () {
        var me = this;
        for (var e in this.proto) {
            if (this.proto.hasOwnProperty(e)) {
                this[e] = (function (e) {
                    return function (handler) {
                        me.socket.send(
                            me.proto[e].apply(me, Ext.Array.slice(arguments, 1)), 
                            Ext.pass(me.proxy, [e, handler], me)
                        );
                    }
                })(e)
            }
        }
    },
    
    handlers: {
        metrics: function (message) {
            this.fireEvent('refresh');
            Ext.each(message, function (agent) {
                this.fireEvent('metrics', agent.name, agent.metrics);
            }, this);
        }
    },
    
    dispatchData: function (data) {
        var handler, tag, message = data.message;
        if (!data.status && this.errorHandler) {
            return this.errorHandler(message ? message.tag : 'dispatch', data);
        }
        if (message && (tag = message.tag)) {
            console.debug("Proxy handler: got a message through '" + tag + "':", message);
            if (handler = this.handlers[tag]) {
                return handler.call(this, message.value);
            }
        }
    },
    
    proxy: function(proto, handler, message) {
        console.debug("Proxy handler: got a response through '" + proto + "':", message);
        if (!message.status && this.errorHandler) {
            return this.errorHandler(proto, message);
        }
        if (handler) {
            return handler(message);
        }
    }
    
});
