package com.lightning.wallet

import com.lightning.wallet.ln.Tools._
import android.widget.{ListView, TextView}
import android.os.Bundle
import android.view.Menu


class LNStartActivity extends ToolbarActivity with ViewSwitch with SearchBar { me =>
  def notifySubTitle(sub: String, infoType: Int) = me runOnUiThread add(sub, infoType).ui
  lazy val lnStartNodesList = findViewById(R.id.lnStartNodesList).asInstanceOf[ListView]
  lazy val lnStartNodeInfo = findViewById(R.id.lnStartNodeInfo).asInstanceOf[TextView]
  lazy val views = lnStartNodesList :: lnStartNodeInfo :: Nil

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
