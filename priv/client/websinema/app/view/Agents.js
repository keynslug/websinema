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

Ext.define('Websinema.view.Agents', {

    extend: 'Ext.panel.Panel',
    alias: 'widget.agentsView',
    
    layout: 'accordion',
    defaults: {
        bodyStyle: 'padding:2px'
    },
    layoutConfig: {
        animate: true,
        activeOnTop: true
    },
    
    items: [
    ],
    
    initComponent: function () {
        console.debug('Initializing agents view...');
        this.callParent(arguments);
    },
    
    newAgent: function (agent) {
        this.add(Ext.create('Ext.tree.Panel', {
            title: agent.get('name'),
            id: agent.get('name'),
            layout: 'fit',
            store: agent.get('metrics'),
            rootVisible: false
        }));
    }

});
