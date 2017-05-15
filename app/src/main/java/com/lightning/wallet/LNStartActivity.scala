package com.lightning.wallet

import android.os.Bundle
import android.view.Menu
import com.lightning.wallet.ln.Tools._


class LNStartActivity extends ToolbarActivity
with ViewSwitch with SearchBar { me =>

  lazy val views =
    findViewById(R.id.lnStartNodesList) ::
      findViewById(R.id.lnStartNodeInfo) :: Nil

  def notifySubTitle(subtitle: String, infoType: Int) =
    me runOnUiThread add(subtitle, infoType).ui

  // Initialize this activity, method is run once
  override def onCreate(savedState: Bundle) =
  {
    super.onCreate(savedState)
    wrap(initToolbar)(me setContentView R.layout.activity_ln_start)
    add("test", Informer.LNSTATE).ui.run
  }

  def react(query: String) = println(query)
  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_start_ops, menu)
    setupSearch(menu)
    true
  }
}
