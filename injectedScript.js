leaderboardChange("3", "102", 2, "0", 0, "Mono");

function toArray(nodeList) { return Array.prototype.slice.call(nodeList); }

var entries = toArray(document.querySelectorAll(".leaderboard_main_table"));
function getStuff() {
    if (!entries.length) {
        return;
    } 
    var entry = entries.shift();
    entry.querySelector(".leaderboard_details_cell a").click();

    setTimeout(function() {
        var stats = document.querySelector("#sb-content").contentDocument
            .querySelector(".user_stats_table");
        var gameData = toArray(stats.firstChild.children).reduce(function(accum, element) {
            var th = element.getElementsByTagName("th")[0];
            var td = element.getElementsByTagName("td")[0];
            accum[th.innerText] = td.innerText;
            return accum;
        }, {});
        console.log(gameData);
        Shadowbox.close();
        setTimeout(getStuff, 1000);
    }, 1000);
}
getStuff();
