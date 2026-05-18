      document.addEventListener('DOMContentLoaded', function() {
        var TAB_PARAM = 'tab';
        var tabContainer = document.getElementById('main_tabs');
        var isHistoryNav = false;
        var currentTab = null;

        function getActiveTab() {
          if (!tabContainer) return null;
          var active = tabContainer.querySelector('.tab-pane.active');
          return active ? active.getAttribute('data-value') : null;
        }

        function syncUrl(tab, replace) {
          if (!tab) return;
          var url = new URL(window.location);
          url.searchParams.set(TAB_PARAM, tab);
          var method = replace ? 'replaceState' : 'pushState';
          window.history[method]({ tab: tab }, '', url);
        }

        function switchToTab(tab, fromPop) {
          if (!tab) return;
          if (fromPop) isHistoryNav = true;
          if (tab === currentTab) {
            if (fromPop) isHistoryNav = false;
            return;
          }
          var btn = document.querySelector('a[data-value="' + tab + '"]');
          if (btn) btn.click();
        }

        // Hook Bootstrap tab events (Shiny uses BS tabs under the hood)
        if (window.jQuery) {
          window.jQuery(document).on('shown.bs.tab', 'a[data-toggle="tab"]', function(e) {
            var tab = window.jQuery(e.target).data('value');
            if (!tab) return;
            if (isHistoryNav) {
              syncUrl(tab, true);
              isHistoryNav = false;
            } else if (tab !== currentTab) {
              syncUrl(tab, false);
            }
            currentTab = tab;
          });
        }

        if (tabContainer) {
          var observer = new MutationObserver(function() {
            var tab = getActiveTab();
            if (!tab) return;
            if (tab === currentTab) {
              if (isHistoryNav) isHistoryNav = false;
              return;
            }
            if (isHistoryNav) {
              syncUrl(tab, true);
              isHistoryNav = false;
            } else {
              syncUrl(tab, false);
            }
            currentTab = tab;
          });
          tabContainer.querySelectorAll('.tab-pane').forEach(function(pane) {
            observer.observe(pane, { attributes: true, attributeFilter: ['class'] });
          });
        }

        var initialTab = new URL(window.location).searchParams.get(TAB_PARAM) || getActiveTab() || 'landing';
        syncUrl(initialTab, true);
        isHistoryNav = true;
        switchToTab(initialTab, true);
        setTimeout(function() { isHistoryNav = false; }, 0);
        currentTab = initialTab;

        window.addEventListener('popstate', function(event) {
          var tabFromState = event.state && event.state.tab;
          var tabFromUrl = new URL(window.location).searchParams.get(TAB_PARAM);
          switchToTab(tabFromState || tabFromUrl || 'landing', true);
        });
      });
