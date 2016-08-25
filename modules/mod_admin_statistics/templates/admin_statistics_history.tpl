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
<svg id="erlang-memory"></svg>


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
			"erlang_io__output",
			"erlang_statistics__run_queue"
		];
	var sortedData = [];
	initSortedData();
	data.forEach(sortMetric);

	var xScale = new Plottable.Scales.Time();
	var yScale = new Plottable.Scales.Linear();

	var xAxis = new Plottable.Axes.Time(xScale, "bottom");
	var yAxis = new Plottable.Axes.Numeric(yScale, "left");

	var plot = new Plottable.Plots.Line();
	plot.x(timeAxisAccessor, xScale);
	plot.y(yAxisAccessor, yScale);

	var panZoom = new Plottable.Interactions.PanZoom();
	panZoom.addXScale(xScale)
			.addYScale(yScale)
			.attachTo(plot);

	var dataset = new Plottable.Dataset(sortedData["erlang_memory__total"]);

	plot.addDataset(dataset);

	var chart = new Plottable.Components.Table([[yAxis, plot], [null, xAxis]]);
	chart.renderTo("svg#erlang-memory");


	$($('#dtp').submit(getStatsByDateTime));


	function getStatsByDateTime(submit) {
		submit.preventDefault();
		$.ajax({
            url     : $(this).attr('action'),
            type    : $(this).attr('method'),
            dataType: 'json',
            data    : $(this).serialize(),
            success : function(data) {
            			initSortedData();
                        $.each(data, sortMetric);
                        dataset.data(sortedData);
                      },
            error   : function( xhr, err ) {
                        alert('Error');     
                      }
	    });


	    //TODO: Get browser timezone and use it in date time query

		function get_time_zone_offset() {
	    	var current_date = new Date();
	    	return -current_date.getTimezoneOffset() / 60;
	    }

	    function standardiseTime (time) {
			time.replace
		}
	}
	
	function timeAxisAccessor(d) {
		var time_recorded = new Date(d.time_recorded);
		return time_recorded;
	}
	function yAxisAccessor(d) {
		//Convert result from B to Mb
		return (d.metric_value/1024/1024).toFixed(1);
	}
	function sortMetric(datapoint) {
		metrics.forEach(function(metric_name) {
			if(metric_name === datapoint.metric_name) {
				sortedData[metric_name].push(datapoint);
			}
		});
	}

	//Empty and Create sub-arrays for categorising metrics in sortedData array
	function initSortedData() {
		sortedData = [];
		//Create sub-arrays within sortedData where the stats will be stored
		metrics.forEach(function(m) { sortedData[m] = []; });
	}

{% endjavascript %}

<br />

<!-- DEBUG STUFF END -->

{% endblock %}
