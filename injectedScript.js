var WAIT_DELAY = 2000;
var username = "CHUCK_NORRIS";

function toArray(nodeList) { return Array.prototype.slice.call(nodeList); }

function waitTill(condObj, callback) {
    if (!condObj.val) {
        setTimeout(waitTill, WAIT_DELAY, condObj);
    }
    else {
        callback();
    }
}

function haveNextPage() {
    var nextButton = toArray(document.querySelectorAll(".leaderboard_table_page_list a")).slice(-1)[0];
    return nextButton.onclick != null;
}

function logData(obj) {
    str = "";
    for (attr in obj) {
        if (attr != "") {
            str += obj[attr] + ",";
        }
        else {
            break;
        }
    }
    str = str.substring(0, str.length - 1);
    console.log(str);
}

function nextPage() {
    toArray(document.querySelectorAll(".leaderboard_table_page_list a")).slice(-1)[0].click();
    setTimeout(function() {
                   entries = toArray(document.querySelectorAll(".leaderboard_main_table"));
                   getStuff();
               },
               2 * WAIT_DELAY);
}

var entries = toArray(document.querySelectorAll(".leaderboard_main_table"));

function getStuff() {

    if (entries.length === 0 && haveNextPage()) {
        console.log("---------next page---------");
        setTimeout(nextPage, 2 * WAIT_DELAY);
    }
    else {
        getNext();
    }

}

function getNext() {
    var entry = entries.shift();
    username = entry.querySelector(".leaderboard_username_cell a").innerText;
    entry.querySelector(".leaderboard_details_cell a").click();

    setTimeout(extractStatsFromShadowbox, WAIT_DELAY);
}

function extractStatsFromShadowbox() {
    var stats = document.querySelector("#sb-content").contentDocument
        .querySelector(".user_stats_table");
    if (stats != null) {
        var gameData = toArray(stats.firstChild.children).reduce(function(accum, element) {
            var th = element.getElementsByTagName("th")[0];
            var td = element.getElementsByTagName("td")[0];
            accum[th.innerText] = td.innerText;
            return accum;
        }, {name: username});
        logData(gameData);
    }
    Shadowbox.close();
    setTimeout(getStuff, WAIT_DELAY);
}

getStuff();
