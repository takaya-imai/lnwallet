package com.lightning.wallet

import android.view.Menu


class LNStartActivity extends ToolbarActivity
with ViewSwitch with SearchBar { me =>

  def notifySubTitle(subtitle: String, infoType: Int) = ???
  lazy val views = ???

  def react(query: String) = println(query)
  override def onCreateOptionsMenu(menu: Menu) = {
    getMenuInflater.inflate(R.menu.ln_start_ops, menu)
    setupSearch(menu)
    true
  }
}
