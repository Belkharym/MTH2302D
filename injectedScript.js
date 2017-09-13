var WAIT_DELAY = 1000;

function toArray(nodeList) { return Array.prototype.slice.call(nodeList); }

function nextPage(callback) {
    toArray(document.querySelectorAll(".leaderboard_table_page_list a")).slice(-1)[0].click();
    setTimeout(callback, WAIT_DELAY);
}

var entries = toArray(document.querySelectorAll(".leaderboard_main_table"));
function getStuff() {
    if (!entries.length) {
        nextPage(function() {
            entries = toArray(document.querySelectorAll(".leaderboard_main_table"));
            getStuff();
        });
    } 
    var entry = entries.shift();
    entry.querySelector(".leaderboard_details_cell a").click();

    setTimeout(function() {
        var stats = document.querySelector("#sb-content").contentDocument
            .querySelector(".user_stats_table");
        if (stats != null) {
            var gameData = toArray(stats.firstChild.children).reduce(function(accum, element) {
                var th = element.getElementsByTagName("th")[0];
                var td = element.getElementsByTagName("td")[0];
                accum[th.innerText] = td.innerText;
                return accum;
            }, {});
            console.log(gameData);
        }
        Shadowbox.close();
        setTimeout(getStuff, WAIT_DELAY);
    }, WAIT_DELAY);
}

leaderboardChange("3", "102", 2, "0", 0, "Mono");
setTimeout(getStuff, WAIT_DELAY);
