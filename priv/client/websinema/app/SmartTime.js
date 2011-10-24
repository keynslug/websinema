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

Ext.define('Websinema.SmartTime', {

    extend: 'Ext.chart.axis.Time',
    alias: 'axis.websinema.smarttime',
    requires: ['Ext.data.Store', 'Ext.data.JsonStore'],

    calculateByLabelSize: true,
    
    dateThreshold: [Ext.Date.SECOND, 5],
    
    constructor: function () {
        this.dateSpecs = {
            order: [
                Ext.Date.MILLI,
                Ext.Date.SECOND, Ext.Date.MINUTE, Ext.Date.HOUR, 
                Ext.Date.DAY, Ext.Date.MONTH, Ext.Date.YEAR 
            ],
            crossed: function (lower, upper, grain) {
                if (Ext.isString(grain)) {
                    grain = this.order.indexOf(grain) + 1;
                }
                if (grain > 0 && grain < this.order.length) {
                    var get = this[this.order[grain]].get;
                    if (get(lower) != get(upper)) {
                        return true;
                    } else {
                        return arguments.callee.call(this, lower, upper, grain + 1);
                    }
                }
                return false;
            }
        };

        this.dateSpecs[Ext.Date.YEAR] = {
            format: 'Y',
            period: 24 * 3600000,
            get: function (d) { return d.getFullYear(); }
        };
        this.dateSpecs[Ext.Date.MONTH] = {
            format: 'M',
            period: 24 * 3600000,
            get: function (d) { return d.getMonth(); }
        };
        this.dateSpecs[Ext.Date.DAY] = {
            format: 'd',
            period: 24 * 3600000,
            get: function (d) { return d.getDate(); }
        };
        this.dateSpecs[Ext.Date.HOUR] = {
            format: 'H',
            period: 3600000,
            get: function (d) { return d.getHours(); }
        };
        this.dateSpecs[Ext.Date.MINUTE] = {
            format: 'i',
            period: 60000,
            get: function (d) { return d.getMinutes(); }
        };
        this.dateSpecs[Ext.Date.SECOND] = {
            format: 's',
            period: 1000,
            get: function (d) { return d.getSeconds(); }
        };
        this.dateSpecs[Ext.Date.MILLI] = {
            format: 'u',
            period: 1,
            get: function (d) { return d.getMilliseconds(); }
        };
        
        this.callParent(arguments);
    },
    
    setLabels: function() {
        var store = this.chart.substore,
            fields = this.fields,
            spec = this.dateSpecs[this.dateThreshold[0]],
            thresh = this.dateThreshold[1] * spec.period,
            format, last, labels, i, dates = this.dates;
        this.labels = labels = [];
        last = Ext.Date.add(dates[0], Ext.Date.MILLI, -1 * (dates[0].getTime() % thresh));
        store.each(function(record, i) {
            
            format = this.dateFormat;
            if (0 == i) {
                return labels.push(Ext.Date.format(dates[i], format));
            }
            if (dates[i].getTime() - last.getTime() >= thresh) {
                if (!this.dateSpecs.crossed(last, dates[i], this.dateThreshold[0])) {
                    format = spec.format;
                }
                labels.push(Ext.Date.format(dates[i], format));
                last = dates[i];
            } else {
                labels.push('');
            }
            
         }, this);
     },
     
     constrainDates: function() {
        var fromDate, nextDate, toDate,
            step = this.step,
            thresh = this.dateThreshold,
            threshPeriod = step[1] * this.dateSpecs[step[0]].period,
            field = this.fields[0],
            store = this.chart.store,
            record, indexLow, indexHigh, space, newStore;
        
        if (!this.constrain) {
            return this.chart.filteredStore = this.chart.store;
        }
        
        fromDate = Ext.Date.add(this.fromDate, Ext.Date.MILLI, -1 * (this.fromDate.getTime() % threshPeriod));
        toDate = Ext.Date.clone(this.toDate);
        newStore = Ext.create('Ext.data.Store', { model: store.model });

        while (+fromDate <= +toDate) {
            
            nextDate = Ext.Date.add(fromDate, step[0], step[1]);
            indexLow = store.findBy(function (r) {
                return +(r.get(field)) >= +fromDate
            });
            indexLow = indexLow < 0 ? store.getCount() : indexLow;

            indexHigh = store.findBy(function (r) {
                return +(r.get(field)) >= +nextDate
            }, store, indexLow);
            indexHigh = indexHigh < 0 ? store.getCount() : indexHigh;
            
            record = {};
            record[field] = fromDate;
            store.model.prototype.fields.each(function(f) {
                
                if (f.name == field) {
                    return;
                }
                record[f.name] = 0;
                if (space = (indexHigh - indexLow)) {
                    for (; indexLow < indexHigh; ++indexLow) {
                        record[f.name] += store.getAt(indexLow).get(f.name);
                    }
                    record[f.name] /= space;
                }
                
            });
            
            newStore.add(record);
            fromDate = nextDate;
            
        }

        this.chart.filteredStore = newStore;
    },
    
    aggregate: function() {
        var dates = [],
            field = this.fields,
            store = this.chart.filteredStore || this.chart.store;
        
        store.each(function (r) {
            dates.push(r.get(field));
        });

        this.chart.substore = store;
        this.dates = dates;
    },

});
