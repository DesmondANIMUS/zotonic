{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}
{% block head_extra  %}
	{% lib "js/plottable/plottable.css" "js/datetimepicker/jquery.datetimepicker.css" "js/d3/d3.min.js" "js/plottable/plottable.min.js" "js/datetimepicker/jquery.datetimepicker.full.min.js" %}
{% endblock %}

{% block content %}

{% include "_admin_stats_menu.tpl" %}

<!-- DEBUG STUFF -->

<form id="dtp" method="get" action="/api/admin_statistics">
	<label for="dtp-start">Start: </label>
  	<input type="text" name="start" id="dtp-start" />

  	<label for="dtp-end">End: </label>
  	<input type="text" name="end" id="dtp-end" />

  	<input type="submit" value="Apply" />
 </form>

<br />
<div id="charts">
</div>


<br />
{% javascript %}

	//Date Time Picker
	$('#dtp-start').datetimepicker();
	$('#dtp-end').datetimepicker();

	//Charting Code
	var data = {{ m.z_stats.datetime[{query beginDatetime="2016-8-3 15:01:46" endDatetime="2016-8-3 15:59:46"}] | to_json }};
	var metrics = [
			"erlang_system_info__process_count",
			"erlang_memory__total",
			"erlang_memory__processes",
			"erlang_memory__processes_used",
			"erlang_memory__system",
			"erlang_memory__atom",
			"erlang_memory__atom_used",
			"erlang_memory__binary",
			"erlang_memory__code",
			"erlang_memory__ets",
			"erlang_network__tcp_port_count",
			"erlang_statistics__run_queue",
			"erlang_io__input",
			"erlang_io__output"
		];

	//create svg elements wherein charts of the metrics will be drawn
	$.each(metrics, function(index, metric) {
		metricElement = '<svg id="' + metric + '"></svg>';
		$('#charts').append(metricElement);
	});

	var sortedData = {};
	initSortedData();
	$.each(data, sortMetric);
	drawChart(sortedData);
	console.log(sortedData);


	$($('#dtp').submit(getStatsByDateTime));


	function getStatsByDateTime(submit) {
		submit.preventDefault();
		$.ajax({
            url     : $(this).attr('action'),
            type    : $(this).attr('method'),
            dataType: 'json',
            data    : $(this).serialize(),
            success : function(newData) {
            			data = [];
            			data = newData;
            			initSortedData();            			
                        $.each(data, sortMetric);
                        dataset.data(sortedData["erlang_memory__total"]);
                      },
            error   : function( xhr, err ) {
                        alert('Error');     
                      }
	    });
	}

	//Draw charts and convert {{sortedData}} to an array of Metric objects
	function drawChart(initSortedData) {
		sortedData = {};
		$.each(initSortedData, function(metricName, metric) {
			sortedData[metricName] = new Metric(metricName, metric.rawData);
			var xAxis = sortedData[metricName].xAxis;
			var yAxis = sortedData[metricName].yAxis;
			var plot = sortedData[metricName].plot;

			sortedData[metricName].chart = new Plottable.Components.Table([[yAxis, plot],[null, xAxis]]);
			elementID = '#' + metricName;
			sortedData[metricName].chart.renderTo(elementID);
		});
	}
	
	//Create a Metric object based on the Plottable library
	function Metric(name, rawData) {
		this.name = name;
		this.rawData = rawData;
		this.xScale = new Plottable.Scales.Time();
		this.yScale = new Plottable.Scales.Linear();
		this.xAxis = new Plottable.Axes.Time(this.xScale, "bottom");
		this.yAxis = new Plottable.Axes.Numeric(this.yScale, "left");
		this.plot = new Plottable.Plots.Line().x(timeAxisAccessor, this.xScale).y(yAxisAccessor, this.yScale);
		this.panZoom = new Plottable.Interactions.PanZoom().addXScale(this.xScale)
			.addYScale(this.yScale)
			.attachTo(this.plot);
		this.chartData = new Plottable.Dataset(this.rawData);
		this.plot.addDataset(this.chartData);
	}
	function timeAxisAccessor(d) {
		var time_recorded = new Date(d.time_recorded);
		return time_recorded;
	}
	function yAxisAccessor(d) {
		//Convert result from B to Mb
		return (d.metric_value/1024/1024).toFixed(1);
	}
	function sortMetric(index, datapoint) {
		metrics.forEach(function(metric_name) {
			if(metric_name === datapoint.metric_name) {
				sortedData[metric_name].rawData.push(datapoint);
			}
		});
	}

	//Empty and Create sub-arrays for categorising metrics in sortedData array
	function initSortedData() {
		sortedData = {};
		//Create sub-objects within sortedData where the stats will be stored
		metrics.forEach(function(m) { 
			sortedData[m] = {};
			sortedData[m].rawData = [];
		});
	}

{% endjavascript %}

<br />

<!-- DEBUG STUFF END -->

{% endblock %}