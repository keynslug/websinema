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

Ext.data.Types.MetricsTree = {
    
    type: 'metricstree',
    
    convert: function (v, data) {

        var conv = function (parent, metrics) {
            if (Ext.isArray(metrics)) {
                return Ext.Array.map(metrics, Ext.pass(conv, [parent]));
            }
            if (Ext.isObject(metrics)) {
                metrics.text = metrics.name.substring(parent ? parent.name.length : 0).replace(/^\W*/, '');
                if (metrics.category == "group") {
                    metrics.children = conv(metrics, metrics.children);
                }
                else {
                    metrics.leaf = true;
                    metrics.checked = false;
                    metrics.id = metrics.name;
                    metrics.store = Ext.create('Ext.data.Store', {
                        model: 'Websinema.model.Measure'
                    });
                    metrics.store.suspendEvents();
                }
                return metrics;
            }
        };
        
        var actual = data.get('metrics');
        if (actual) {
            actual.setRootNode({
                expanded: true,
                children: conv(null, v)
            });
            return actual;
        } else {
            return v;
        }
        
    },
    
    sortType: function () {
        return 0;
    }
    
};

Ext.define('Websinema.model.Agent', {
    extend: 'Ext.data.Model',
    fields: [
        {name: 'name', type: 'string'},
        {name: 'active', type: 'boolean', defaultValue: true},
        {
            name: 'metrics', 
            type: Ext.data.Types.MetricsTree, 
            defaultValue: Ext.create('Ext.data.TreeStore', {
                root: { children: [] }
            })}
    ],
    
    addMetric: function (metric, where) {
        var node = where || this.get('metrics').getNodeById(metric.name);
        if (node) {
            var store = node.raw.store;
            if (Ext.isArray(metric.values)) {
                for (var i = metric.values.length - 1; i >= 0; --i) {
                    arguments.callee.call(this, metric.values[i], node);
                }
                return;
            }
            var count, model = {
                date: metric.ts,
                value: metric.value
            };
            switch (node.raw.type) {
                case 'integer':
                    store.add(model);
                    count = store.count();
                    if (count > 1024) {
                        store.removeAt(0);
                    } 
                break;
                default:
                    if (store.count()) {
                        store.first().set(model);
                    } else {
                        store.add(model);
                    }
                break;
            }
        }
    }
});
