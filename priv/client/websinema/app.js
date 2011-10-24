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

Ext.require([
    'Ext.app.Application',
    'Ext.app.Controller',
    'Ext.window.MessageBox',
    'Ext.container.Viewport'
]);

Ext.onReady(function () {
    console.debug("Starting application...");
    
    var connectSucceed = function () {
        Ext.application({
        
            name: 'Websinema',
            appFolder: 'app',

            controllers: [
                'Layout'
            ],
        
            launch: function () {
                Ext.create('Ext.container.Viewport', {
                    layout: 'fit',
                    items: { xtype: 'appLayout' }
                });
            },
            
            reportError: function (through, message) {
                var text = Ext.String.format(
                    'Error occured while requesting server through websocket:' + 
                    '<br /><br />Request: <b>{0}</b><br />Reason: <b>{1}</b><br />Payload:<br /><pre>{2}</pre>',
                    through, message.reason, message.origin
                );
                Ext.Msg.show({
                    title: 'Data Transfer Error',
                    msg: text,
                    buttons: Ext.Msg.OK,
                    animateTarget: 'messageSource',
                    icon: Ext.window.MessageBox.ERROR
                });
            }
            
        });
    }
    
    var listeners = {
        online: connectSucceed,
        offline: function (reason) {
            Ext.Msg.alert('Error', 'The application failed to start since websockets are ' + reason);
        }
    };
    
    Websinema.Websocket.init(listeners);
});
