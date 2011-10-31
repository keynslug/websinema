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

Ext.define('Websinema.view.Charts', {

    extend: 'Ext.panel.Panel',
    alias: 'widget.chartsView',
    
    requires: [
        'Ext.button.Cycle',
        'Ext.panel.Panel',
        'Websinema.SmartTime',
        'Ext.chart.Chart',
        'Ext.chart.series.Line',
        'Ext.chart.axis.Numeric',
        'Ext.chart.theme.Base',
        'Ext.toolbar.TextItem'
    ],
    
    autoScroll: true,

    bodyCls: 'chartsView',    
    
    tbar: [
        {xtype: 'cycle', id: 'refreshCycle', prependText: 'Refresh interval is ', showText: true, menu: {
            items: [
                {text: '1 sec', checked: true, value: 1000},
                {text: '5 sec', value: 5000},
                {text: '10 sec', value: 10000},
                {text: '15 sec', value: 15000},
                {text: 'Suspend', value: 0}
            ]
        }}
    ],
    
    items: [
    ],
    
    renderers: {
        
        header: {
            height: 24,
            create: function (agentName, metric) {
                return Ext.create('Ext.Component', {
                    height: 24,
                    border: 0,
                    cls: 'chartHeader',
                    html: Ext.String.format('<div class="chartLabel">{0}::{1}<div class="after"></div></div>', agentName, metric.id)
                });
            }
        },
        
        container: {
            create: function () {
                return Ext.create('Ext.panel.Panel', {
                    flex: 1,
                    border: 0,
                    layout: 'fit',
                    cls: 'chartContainer',
                    items: []
                });
            }
        },
        
        placeholder: {
            create: function (height) {
                var h = height - 24 * 2;
                return Ext.create('Ext.Component', {
                    border: 0,
                    height: 0,
                    placeholderRendered: false,
                    cls: 'chartNaPlaceholder',
                    html: Ext.String.format(
                        '<div class="chartNaPlaceholderInner" style="height:{0}px; line-height:{0}px">not accessible</div>', 
                        h
                    )
                });
            },
            render: function (object, state) {
                object.placeholderRendered = state;
                new Ext.fx.Anim({
                    target: object,
                    duration: 1000,
                    to: {
                        opacity: state ? 0.7 : 0.0
                    }
                });
            }
        },
        
        chart: {
            height: 360,
            scope: 60,
            context: this,
            bounds: function (store, interval) {
                var toDate, fromDate, fresh = store.last();
                toDate = fresh ? fresh.get('date') : Ext.Date.add(new Date(), Ext.Date.SECOND, -1);
                fromDate = Ext.Date.add(toDate, Ext.Date.MILLI, -interval * this.scope);
                return [fromDate, toDate];
            },
            refresh: function (object, interval) {
                var ax = object.axes.get(1), 
                    bounds = this.bounds(object.store, interval);
                ax.fromDate = bounds[0];
                ax.toDate = bounds[1];
                ax.step = [Ext.Date.SECOND, interval / 1000];
                ax.dateThreshold = [Ext.Date.SECOND, 5];
                if (interval > 1000 && interval < 5000) {
                    ax.dateThreshold = [Ext.Date.SECOND, 5];
                }                
                if (interval >= 5000 && interval < 10000) {
                    ax.dateThreshold = [Ext.Date.SECOND, 10];
                }
                if (interval >= 10000) {
                    ax.dateThreshold = [Ext.Date.SECOND, 30];
                }
                object.redraw();
            },
            create: function (agentName, metric) {
                var bounds = this.bounds(metric.store, 1000);
                return Ext.create('Ext.chart.Chart', {
                    store: metric.store,
                    animate: false,
                    shadow: false,
                    theme: 'Websinema',
                    insetPadding: 16,
                    axes: [{
                        type: 'Numeric',
                        title: 'value',
                        position: 'right',
                        fields: 'value',
                        minimum: 0,
                        grid: {
                            odd: {
                                opacity: 0.6,
                                fill: '#ddd',
                                stroke: '#bbb'
                            }
                        }
                    }, {
                        type: 'Websinema.SmartTime',
                        title: 'timestamp',
                        position: 'bottom',
                        dateFormat: 'H:i:s',
                        fields: 'date',
                        groupBy: 'hour,minute,second',
                        aggregateOp: 'avg',
                        step: [Ext.Date.SECOND, 1],
                        constrain: true,
                        fromDate: bounds[0],
                        toDate: bounds[1]
                    }],
                    series: [{
                        type: 'line',
                        axis: 'right',
                        xField: 'date',
                        yField: 'value',
                        fill: true,
                        showMarkers: true,
                        smooth: 3,
                        markerConfig: {
                            type: 'circle',
                            radius: 1
                        },
                        tips: {
                            trackMouse: true,
                            height: 24,
                            renderer: function (storeItem) {
                                this.setTitle(storeItem.get('value'));
                            }
                        }
                    }]
                });
            }
        },
        
        clock: {
            height: 86,
            refresh: function (object) {
                var dom = Ext.getDom(object.el);
                var present = object.userTemplate(object.store.last());
                dom.innerHTML = Ext.String.format('<div class="genericLabel clock">{0}</div>', present);
            },
            create: function (agentName, metric) {
                var odd = true;
                var template = function (data) {
                    var i = 3, piece, ticks, result = '';
                    if (data) {
                        ticks = Math.floor(data.data.value / 100);
                    }
                    else {
                        return '00:00:00';
                    }
                    while (i) {
                        piece = Ext.String.leftPad((i == 1 ? ticks : (ticks % 60)).toString(), 2, '0');
                        result = piece + (i == 3 ? '' : (odd ? ':' : '.')) + result;
                        ticks = Math.floor(ticks / 60);
                        --i;
                    }
                    odd = !odd;
                    return result;
                };
                var clock = Ext.create('Ext.Component', {
                    store: metric.store,
                    userTemplate: template,
                    cls: 'labelContainer'
                });
                clock.addListener("render", function (object) {
                    this.refresh(object);
                    return true;
                }, this, {single: true});
                return clock;
            }
        },
        
        label: {
            height: 86,
            create: function (agentName, metric) {
                var template = function (data) {
                    if (data) {
                        if (Ext.isArray(data.data.value)) {
                            return data.data.value.join('.');
                        }
                        return data.data.value.toString();                        
                    }
                    else {
                        return 'undefined';
                    }
                };
                return Ext.create('Ext.Component', {
                    html: Ext.String.format('<div class="genericLabel">{0}</div>', template(metric.store.last())),
                    cls: 'labelContainer'
                });
            }
        },
        
        datetime: {
            height: 86,
            create: function (agentName, metric) {
                var template = function (data) {
                    if (data) {
                        var date = new Date(data.data.value * 1000);
                        return Ext.Date.format(date, 'F d, Y, H:i:s (O P)');
                    }
                    else {
                        return 'somewhen';
                    }
                };
                return Ext.create('Ext.Component', {
                    html: Ext.String.format('<div class="genericLabel">{0}</div>', template(metric.store.last())),
                    cls: 'labelContainer'
                });
            }
        },
        
        byType: {
            integer: 'chart',
            ticks: 'clock',
            string: 'label',
            object: 'label',
            timestamp: 'datetime'
        }
        
    },
    
    initComponent: function () {
        console.debug('Initializing charts view...');
        this.initTheme();
        this.callParent(arguments);
    },
    
    initTheme: function () {
        var labelFont = '10px Arial, Helvetica, sans-serif',
            titleFont = 'bold 13px Arial, Helvetica, sans-serif';
        Ext.define('Ext.chart.theme.Websinema', {
            extend: 'Ext.chart.theme.Base',
            constructor: function(config) {
                this.callParent([Ext.apply({
                    axisLabelLeft: {
                        font: labelFont
                    },
                    axisLabelTop: {
                        font: labelFont
                    },
                    axisLabelRight: {
                        font: labelFont
                    },
                    axisLabelBottom: {
                        font: labelFont
                    },
                    axisTitleLeft: {
                        font: titleFont
                    },
                    axisTitleTop: {
                        font: titleFont
                    },
                    axisTitleRight: {
                        font: titleFont
                    },
                    axisTitleBottom: {
                        font: titleFont
                    }
                }, config)]);
            }
        });
    },
    
    renderComponents: {},
    
    createRenderer: function (agentName, metric) {
        var type = this.renderers.byType[metric.type],
            renderer, rs = this.renderComponents;
        if (rs[agentName] === undefined) {
            rs[agentName] = {};
        }
        if (type && rs[agentName][metric.id] === undefined) {
            rs[agentName][metric.id] = renderer = {}; 
            renderer.type = type;
            renderer.metric = metric;
            renderer.owner = Ext.create('Ext.panel.Panel', {
                border: 0,
                layout: {
                    type: 'vbox',
                    align: 'stretch'
                },
                items: [],
                height: this.renderers[type].height
            });
            renderer.header = this.renderers.header.create(agentName, metric);
            renderer.placeholder = this.renderers.placeholder.create(this.renderers[type].height);
            renderer.container = this.renderers.container.create();
            renderer.object = this.renderers[type].create(agentName, metric);
            renderer.owner.add(renderer.header);
            renderer.owner.add(renderer.placeholder);
            renderer.owner.add(renderer.container);
            renderer.container.add(renderer.object);
            this.add(renderer.owner);
        }
    },
    
    destroyRenderer: function (agentName, id) {
        var renderer, rs = this.renderComponents;
        if (rs[agentName] && (renderer = rs[agentName][id])) {
            renderer.owner.destroy();
            delete rs[agentName][id];
        }
    },
    
    refreshRenderers: function (interval) {
        var o, r, rs = this.renderComponents;
        for (var i in rs) {
            if (rs.hasOwnProperty(i)) {
                for (var j in rs[i]) {
                    if (rs[i].hasOwnProperty(j)) {
                        o = rs[i][j];
                        r = this.renderers[o.type]
                        if (o.object && r.refresh) {
                            if (o.metric) {
                                if (o.metric.store.notAccessible ^ o.placeholder.placeholderRendered) {
                                    this.renderers.placeholder.render(o.placeholder, o.metric.store.notAccessible);
                                }
                            }
                            r.refresh(o.object, interval);
                        }
                    }
                }
            }
        }
    }
    
});
