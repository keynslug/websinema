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

Ext.define('Websinema.controller.Layout', {
    
    extend: 'Ext.app.Controller',
    requires: ['Websinema.Proto'],
    
    models: [
        'Agent'
    ],
    
    views: [
        'Agents',
        'Charts',
        'Layout'
    ],
    
    stores: [
        'Agents'
    ],
    
    refs: [
        {ref: 'agentsViewObject', selector: 'agentsView'},
        {ref: 'chartsViewObject', selector: 'chartsView'}
    ],
    
    init: function () {
        console.debug("Initializing layout controller...");
        
        this.proto = Ext.create('Websinema.Proto', {
            name: 'Layout',
            errorHandler: this.application.reportError
        });
        
        this.initStoreEvents();
        this.initControl();
        
        this.proto.on('metrics', this.handleMetrics, this);
        this.proto.on('refresh', this.handleRefresh, this);
        Websinema.Websocket.on('online', function () { 
            this.proto.getAgents(Ext.bind(this.handleAgents, this));
        }, this)
    },
    
    handleAgents: function (data) {
        this.getAgentsStore().add(Ext.Array.map(data.message, function (e) {
            this.proto.discoverAgent(Ext.bind(this.handleDiscovery, this), e);
            return {name: e, active: true};
        }, this));
    },
    
    handleDiscovery: function (data) {
        var agent = this.getAgentsStore().findRecord('name', data.message.name);
        if (agent) {
            agent.set('metrics', data.message.metrics);
        }
    },
    
    handleRefresh: function () {
        if (this.refreshInterval) {
            this.doRefresh(this.refreshInterval);
        }
    },
    
    handleMetrics: function (name, metrics) {
        var agent = this.getAgentsStore().findRecord('name', name);
        Ext.each(metrics, function (m) {
            agent.addMetric(m);
        });
    },
    
    subscription: {},
    
    resubscribe: function () {
        this.proto.subscribe(Ext.bind(this.handleRefresh, this), this.refreshInterval);
    },
    
    subscribe: function (name, metric) {
        var uid = name + "::" + metric.id;
        if (this.subscription[uid]) {
            console.error("Already subscribed on " + uid);
        }
        else {
            console.debug("Subscribing on: ", uid);
            this.proto.subscribe(Ext.bind(function (message) {
                var backlog = message.message.value[0];
                this.handleMetrics(backlog.name, backlog.metrics);
                this.subscription[uid] = metric;
                this.getChartsViewObject().createRenderer(name, metric);
            }, this), this.refreshInterval, name, metric.sid, true);
        }
    },
    
    unsubscribe: function (name, metric) {
        var uid = name + "::" + metric.id;
        if (this.subscription[uid]) {
            console.debug("Unsubscribing from: ", uid);
            this.proto.subscribe(Ext.bind(function () {
                this.getChartsViewObject().destroyRenderer(name, metric.id);
                this.subscription[uid] = undefined;
            }, this), this.refreshInterval, name, metric.sid, false);
        }
        else {
            console.error("Not subscribed on " + uid);
        }
    },
    
    initControl: function () {
        this.control({
            
            'appLayout button#agentsCollapse' : {
                click: Ext.bind(function () {
                    this.getAgentsViewObject().toggleCollapse();
                }, this)
            },
            
            'agentsView treepanel' : {
                itemclick: Ext.bind(this.activateItem, this)
            },
            
            'chartsView #refreshCycle' : {
                change: Ext.bind(this.resetInterval, this)
            }
            
        });
    },
    
    initStoreEvents: function () {
        this.getAgentsStore().on({
        
            'add': Ext.bind(function (store, records) {
                var view = this.getAgentsViewObject();
                for (var i in records) {
                    view.newAgent(records[i]);
                }
            }, this)
            
        });
    },
    
    activeRefresh: null,
    refreshInterval: 1000,
    resendInterval: 1000,
    
    initRefresh: function () {
        console.debug("Starting active refresh sequence... ", this.refreshInterval);
        var interval = this.refreshInterval;
        var refresh = function (i) {
            if (this.refreshInterval != i) {
                this.cancelRefresh();
                this.initRefresh();
            }
        };
        this.cancelRefresh();
        this.doRefresh(interval);
        this.activeRefresh = setInterval(
            Ext.bind(function () { refresh.call(this, interval); }, this),
            interval
        );
    },
    
    doRefresh: function (interval) {
        var view = this.getChartsViewObject();
        if (view) {
            view.refreshRenderers(interval);
        }
    },
    
    cancelRefresh: function () {
        if (this.activeRefresh) {
            console.debug("Cancelling active refresh sequence...");
            clearInterval(this.activeRefresh);
            this.activeRefresh = null;
        }
    },
    
    activateItem: function (view, node, item, index, e) {
        if (e.getTarget('input[role=checkbox]')) {
            var metric = node.raw, name = view.up('treepanel').getId();
            if (metric && name) {
                if (node.data.checked) {
                    this.subscribe(name, metric);
                }
                else {
                    this.unsubscribe(name, metric);
                }
            }
        }
    },
    
    resetInterval: function (cycle, item) {
        this.refreshInterval = item.value;
        this.resubscribe();
    }
    
});

