
// Utilities
function updateElementBody(element, newBody) {
    element.update(newBody);
}

// Register global AJAX handlers to show progress
Ajax.Responders.register({
  onCreate: function() {
	    $('ajax-progress').innerHTML = "<img src='/pub/images/progress.gif'>";
	}, 
  onComplete: function() {
	    $('ajax-progress').innerHTML = "";
	}
});

function onActionSuccess(transport, json) {
    // Update dirty widgets
    var dirtyWidgets = json['widgets'];
    for(var i in dirtyWidgets) {
	var widget = $(i);
	if(widget) {
	    updateElementBody(widget, dirtyWidgets[i]);
	}
    }

    // Perform a series of specialized operations
    var onLoadCalls = json['on-load'];
    if(onLoadCalls) {
	onLoadCalls.each(function(item)
			 {
			     item.evalJSON().call();
			 });
    }
}

function onActionFailure() {
    alert('Could not complete the request. This probably means your session has timed out. Please refresh the page and try again.');
}

function getActionUrl(actionCode, sessionString, isPure) {
    var url = '/?' + sessionString + '&action=' + actionCode;
    if(isPure) {
	url += '&pure=true';
    }
    return url;
}

function initiateAction(actionCode, sessionString) {
    new Ajax.Request(getActionUrl(actionCode, sessionString),
		     {
			 method: 'get',
			 onSuccess: onActionSuccess,
			 onFailure: onActionFailure
		     });
}

function initiateFormAction(actionCode, form, sessionString) {
    // Hidden "action" field should not be serialized on AJAX
    var serializedForm = form.serialize(true);
    delete(serializedForm['action']);
    
    new Ajax.Request(getActionUrl(actionCode, sessionString),
		     {
			 method: form.method,
			 onSuccess: onActionSuccess,
			 onFailure: onActionFailure,
			 parameters: serializedForm
		     });
}

function disableIrrelevantButtons(currentButton) {
    $(currentButton.form).getInputs('submit').each(function(obj)
						   {
						       obj.disable();
						       currentButton.enable();
						   });
}

// Support suggest control
function replaceDropdownWithSuggest(inputId, inputName, choicesId, value) {
    var dropdownOptions = $(inputId).childElements();
    var suggestOptions = [];
    dropdownOptions.each(function(i)
			 {
			     suggestOptions.push(i.innerHTML);
			 });

    var inputBox = '<input type="text" id="' + inputId + '" name="' + inputName + '" class="suggest"';
    if(value) {
	inputBox += 'value="' + value +'"';
    }
    inputBox += '/>';
    
    var suggestHTML = inputBox + '<div id="' + choicesId + '" class="suggest"></div>';
    $(inputId).replace(suggestHTML);
    
    new Autocompleter.Local(inputId, choicesId, suggestOptions, {});
}

function declareSuggest(inputId, choicesId, actionCode, sessionString) {
    new Ajax.Autocompleter(inputId, choicesId, getActionUrl(actionCode, sessionString, true), {});
}
