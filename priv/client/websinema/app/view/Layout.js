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

Ext.define('Websinema.view.Layout', {
    
    extend: 'Ext.panel.Panel',
    alias: 'widget.appLayout',
    requires: [
        'Ext.layout.container.Border',
        'Websinema.view.Agents'
    ],
    
    layout: 'fit',
    
    items: [
        {
            xtype: 'panel',
            layout: 'border',
            flex: 1,
            tbar: [
                {xtype: 'tbtext', text: 'Websinema', id: 'messageSource'},
                '->',
                {xtype: 'button', text: 'Home'},
                {xtype: 'button', text: 'Refresh'},
                {xtype: 'button', text: 'Collapse', id: 'agentsCollapse', enableToggle: true}
            ],
            items: [
                {
                    xtype: 'chartsView',
                    region: 'center',
                    margins: '5 5 5 5'
                },
                {
                    title: 'Agents',
                    xtype: 'agentsView',
                    region: 'west',
                    width: '30%',
                    margins: '5 0 5 5',
                    collapsible: true,
                    animCollapse: true
                }
            ]
        }
    ],
    
    initComponent: function () {
        console.debug("Initializing layout...");
        this.callParent(arguments);
    }
    
});
