var WAIT_DELAY = 1000;

function toArray(nodeList) { return Array.prototype.slice.call(nodeList); }

function waitTill(condObj, callback) {
    if (!condObj.val) {
        setTimeout(waitTill, WAIT_DELAY, condObj);
    }
    else {
        callback();
    }
}

function nextPage() {
    console.log("nextPage-ing");
    toArray(document.querySelectorAll(".leaderboard_table_page_list a")).slice(-1)[0].click();
    setTimeout(function() {
                   entries = toArray(document.querySelectorAll(".leaderboard_main_table"));
                   console.log("entries: ", entries);
                   getStuff();
               },
               2 * WAIT_DELAY);
}

var entries = toArray(document.querySelectorAll(".leaderboard_main_table"));

function getStuff() {
    console.log("Next iteration.");
    console.log("Curr length: ", entries.length);

    if (entries.length === 0) {
        setTimeout(nextPage, 2 * WAIT_DELAY);
    }
    else {
        getNext();
    }

}

function getNext() {
    var entry = entries.shift();
    entry.querySelector(".leaderboard_details_cell a").click();
    console.log("entry: ", entry);

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
        }, {});
        console.log("Data:", gameData);
    }
    else {
        console.log("stats were null");
    }
    Shadowbox.close();
    setTimeout(getStuff, WAIT_DELAY);
}

getStuff();
