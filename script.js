function trigger() {
	$(this).parent().nextUntil('.modrow').toggle();
	$(this).toggleClass('expander');
	$(this).toggleClass('collapser');
}

$(document).ready(function() {
	console.log('foo');
	$('.expander').click(trigger);

	// Create bars
	var max = $('.count').first().text();
	console.log('Max is',max);
	$('tbody tr').each(function () {
		var count = $(this).find('.count').first().text();
		$(this).find('.graph').append(
			$('<div/>', {
				class: 'bar',
				style: "width: " + (count*100/max) + "%",
				text: ' '
			}));

		console.log('This is', count);
	});
});
